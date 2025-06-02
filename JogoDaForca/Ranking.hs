-- Ranking.hs
-- | Gerencia o sistema de pontuação e ranking do Jogo da Forca.
-- Este módulo é responsável por persistir e recuperar informações sobre
-- partidas anteriores, calcular pontuações e exibir históricos de jogos.
module Ranking (
  PontuacaoJogador,            -- Tipo que representa a pontuação de um jogador
  arquivoHistoricoPartidas,    -- Caminho para o arquivo de histórico de partidas
  arquivoRankingAcumulado,     -- Caminho para o arquivo de ranking acumulado
  lerHistoricoPartidas,        -- Lê o histórico de partidas do arquivo
  lerRankingAcumulado,         -- Lê o ranking acumulado do arquivo
  salvarPontuacao,             -- Salva a pontuação de um jogador
  exibirRankingGeral,          -- Exibe o ranking geral
  exibirHistoricoPartidas      -- Exibe o histórico de partidas
) where

import System.IO
import Tipos ( Jogo, EstadoJogo(Ganhou, Perdeu), -- Importa Jogo (abstrato)
               palavraSecretaJogo, estadoJogoJogo ) -- Getters
import LogicaJogo (calcularPontuacao)
import Utilitarios (trim)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)
import Control.Monad (unless)
import Control.Exception (evaluate)

-- | Tipo que representa a pontuação de um jogador: (nome, pontos, resultado, palavra)
-- Esta tupla contém todas as informações sobre uma partida específica.
-- nome: Nome do jogador
-- pontos: Pontuação obtida na partida
-- resultado: "venceu" ou "perdeu"
-- palavra: A palavra secreta da partida
type PontuacaoJogador = (String, Int, String, String)

-- | Caminho para o arquivo de histórico de partidas
-- Este arquivo armazena detalhes de cada partida individual
arquivoHistoricoPartidas :: FilePath
arquivoHistoricoPartidas = "data/historico_partidas.txt"

-- | Caminho para o arquivo de ranking acumulado
-- Este arquivo armazena a pontuação total acumulada de cada jogador
arquivoRankingAcumulado :: FilePath
arquivoRankingAcumulado = "data/ranking_acumulado.txt"

-- | Garante que um arquivo existe antes de tentar ler/escrever nele
-- Atualmente é uma implementação vazia (stub), mas poderia ser expandida
-- para criar o arquivo se ele não existir.
garantirArquivoExiste :: FilePath -> IO ()
garantirArquivoExiste _arquivo = return () -- Implementação vazia, poderia criar o arquivo se necessário

-- | Lê um arquivo de forma segura, capturando possíveis erros
-- Esta função tenta ler o conteúdo de um arquivo e avalia estritamente
-- o resultado para garantir que erros de leitura sejam detectados imediatamente.
-- Se ocorrer um erro, retorna uma string vazia silenciosamente.
lerArquivoSeguro :: FilePath -> IO String
lerArquivoSeguro arquivo = catchIOError strictReadFileHandler handler
  where
    -- Força a avaliação do conteúdo para garantir que erros de leitura sejam detectados imediatamente
    strictReadFileHandler :: IO String
    strictReadFileHandler = do
        conteudo <- readFile arquivo
        _ <- evaluate (length conteudo)  -- Força avaliação estrita do conteúdo
        return conteudo
    -- Manipulador de erros que retorna uma string vazia em caso de erro
    handler :: IOError -> IO String
    handler _err = do
        -- Comentado para não poluir a saída com mensagens de erro
        -- putStrLn $ "[AVISO Ranking.hs] Não foi possível ler o arquivo '" ++ arquivo ++ "'. Erro: " ++ show _err
        return ""  -- Retorna string vazia em caso de erro

-- | Acrescenta conteúdo a um arquivo de forma segura
-- Tenta adicionar texto ao final de um arquivo existente, capturando possíveis erros.
-- Retorna True se bem-sucedido, False se ocorrer um erro.
acrescentarArquivoSeguro :: FilePath -> String -> IO Bool
acrescentarArquivoSeguro arquivo conteudo = do
    resultado <- catchIOError
        (appendFile arquivo conteudo >> return True)  -- Tenta acrescentar o conteúdo
        (\e -> putStrLn ("Erro ao acrescentar ao arquivo: " ++ arquivo ++ " - " ++ show e) >> return False)  -- Trata erro
    return resultado

-- | Escreve conteúdo em um arquivo de forma segura
-- Tenta sobrescrever o conteúdo de um arquivo, capturando possíveis erros.
-- Retorna True se bem-sucedido, False se ocorrer um erro.
escreverArquivoSeguro :: FilePath -> String -> IO Bool
escreverArquivoSeguro arquivo conteudo = do
    resultado <- catchIOError
        (writeFile arquivo conteudo >> return True)  -- Tenta escrever o conteúdo
        (\e -> putStrLn ("Erro ao escrever arquivo: " ++ arquivo ++ " - " ++ show e) >> return False)  -- Trata erro
    return resultado

-- | Analisa uma linha do arquivo de pontuação e retorna um par (nome, pontos)
-- Extrai o nome do jogador e sua pontuação acumulada de uma linha do arquivo de ranking.
-- Formato esperado: "nome_do_jogador pontuacao"
parseLinhaPontuacao :: String -> Maybe (String, Int)
parseLinhaPontuacao linha =
    let ws = words linha  -- Divide a linha em palavras
    in if null ws then Nothing else  -- Se não há palavras, retorna Nothing
        let pontosStr = last ws  -- A última palavra deve ser a pontuação
            nomeParts = init ws  -- Todas as palavras anteriores formam o nome
        in if null nomeParts then Nothing else  -- Se não há nome, retorna Nothing
           case readMaybe pontosStr :: Maybe Int of  -- Tenta converter a pontuação para Int
                Just pontos -> Just (unwords nomeParts, pontos)  -- Se conseguir, retorna o par (nome, pontos)
                Nothing -> Nothing  -- Se a conversão falhar, retorna Nothing

-- | Analisa uma linha do arquivo de histórico e retorna uma PontuacaoJogador
-- Extrai informações detalhadas sobre uma partida de uma linha do histórico.
-- Formato esperado: "nome_do_jogador pontuacao (resultado - palavra: palavra_secreta)"
parseLinhaHistorico :: String -> Maybe PontuacaoJogador
parseLinhaHistorico linha =
    let ws = words linha  -- Divide a linha em palavras
    in if length ws < 6 then Nothing else  -- Precisa de pelo menos 6 palavras para ser válido
        let numWords = length ws
            palavraComParenteses = ws !! (numWords - 1)  -- Última palavra (contém a palavra secreta)
            pontosStr = ws !! (numWords - 5)  -- Pontuação (5 palavras antes do final)
            nomeParts = take (numWords - 5) ws  -- Nome do jogador (tudo antes da pontuação)
            restoDetalhes = unwords (drop (numWords - 4) ws)  -- Detalhes do resultado
        in if null nomeParts || (ws !! (numWords - 2)) /= "palavra:" || (ws !! (numWords - 3)) /= "-"
           then Nothing  -- Formato inválido
           else case readMaybe pontosStr :: Maybe Int of  -- Tenta converter a pontuação para Int
                Just pontos ->
                    let nome = unwords nomeParts  -- Junta as partes do nome
                        resultado = if "(venceu" `isPrefixOf` restoDetalhes then "venceu" else "perdeu"  -- Determina o resultado
                        strAposPalavraKeyword = snd $ breakSubstring "palavra:" restoDetalhes  -- Extrai a parte após "palavra:"
                        palavraComLixo = if null strAposPalavraKeyword then "" else drop (length "palavra: ") strAposPalavraKeyword  -- Remove "palavra: "
                        palavraLimpa = trim $ filter (\c -> c /= ')' && c /= '(') palavraComLixo  -- Limpa a palavra de caracteres extras
                    in if not (null palavraLimpa) &&
                         ((resultado == "venceu" && "(venceu" `isPrefixOf` restoDetalhes) || (resultado == "perdeu" && "(perdeu" `isPrefixOf` restoDetalhes))
                        then Just (nome, pontos, resultado, palavraLimpa)  -- Retorna os dados da partida
                        else Nothing  -- Formato inválido
                Nothing -> Nothing  -- Pontuação inválida
  where
    -- Verifica se uma string é prefixo de outra
    isPrefixOf prefix str = take (length prefix) str == prefix
    -- Divide uma string em duas partes: antes e depois da primeira ocorrência de uma substring
    breakSubstring :: String -> String -> (String, String)
    breakSubstring needle haystack = go [] haystack
      where
        go acc [] = (reverse acc, [])  -- Não encontrou a substring
        go acc src@(c:cs)
          | needle `isPrefixOf` src = (reverse acc, src)  -- Encontrou a substring
          | otherwise               = go (c:acc) cs  -- Continua procurando

-- | Atualiza a pontuação de um jogador no ranking
-- Adiciona a nova pontuação à pontuação acumulada existente, ou cria um novo registro
-- se o jogador não existir no ranking.
-- Implementação recursiva que percorre a lista de jogadores procurando uma correspondência.
atualizarPontuacaoJogador :: String -> Int -> [(String, Int)] -> [(String, Int)]
atualizarPontuacaoJogador nome novaPontuacao [] = [(nome, novaPontuacao)]  -- Caso base: jogador não encontrado, adiciona novo
atualizarPontuacaoJogador nome novaPontuacao ((jogador, pontos):resto)
    | jogador == nome = (jogador, pontos + novaPontuacao) : resto  -- Jogador encontrado, atualiza pontuação
    | otherwise = (jogador, pontos) : atualizarPontuacaoJogador nome novaPontuacao resto  -- Continua procurando

-- | Lê o histórico de partidas do arquivo
-- Carrega e analisa todas as partidas registradas no arquivo de histórico.
lerHistoricoPartidas :: IO [PontuacaoJogador]
lerHistoricoPartidas = do
    garantirArquivoExiste arquivoHistoricoPartidas
    conteudo <- lerArquivoSeguro arquivoHistoricoPartidas
    let linhas = lines conteudo  -- Divide o conteúdo em linhas
    return (mapMaybe parseLinhaHistorico linhas)  -- Analisa cada linha, ignorando as inválidas

-- | Lê o ranking acumulado do arquivo
-- Carrega e analisa a pontuação acumulada de todos os jogadores.
lerRankingAcumulado :: IO [(String, Int)]
lerRankingAcumulado = do
    garantirArquivoExiste arquivoRankingAcumulado
    conteudo <- lerArquivoSeguro arquivoRankingAcumulado
    let linhas = lines conteudo  -- Divide o conteúdo em linhas
    return (mapMaybe parseLinhaPontuacao linhas)  -- Analisa cada linha, ignorando as inválidas

-- | Registra o resultado de uma partida no histórico e atualiza o ranking
-- Esta função é chamada quando uma partida termina (vitória ou derrota).
-- Registra os detalhes da partida no histórico e atualiza a pontuação acumulada do jogador.
registrarResultadoPartida :: String -> Jogo -> Bool -> IO Bool
registrarResultadoPartida nomeJogador jogo ganhou = do
    garantirArquivoExiste arquivoHistoricoPartidas
    -- Calcula a pontuação: pontos se ganhou, 0 se perdeu
    let pontuacao = if ganhou then calcularPontuacao jogo else 0
    let resultado = if ganhou then "venceu" else "perdeu"
    -- Formata a linha para o arquivo de histórico
    let linha = nomeJogador ++ " " ++ show pontuacao ++ " (" ++ resultado ++ " - palavra: " ++ palavraSecretaJogo jogo ++ ")\n"
    
    -- Tenta salvar no histórico de partidas
    sucessoHistorico <- acrescentarArquivoSeguro arquivoHistoricoPartidas linha
    
    if not sucessoHistorico
        then do
            putStrLn "Erro: Não foi possível salvar no histórico de partidas."
            return False
        else do
            -- Tenta atualizar o ranking acumulado
            sucessoRanking <- atualizarRankingSimplesAcumulado nomeJogador pontuacao
            unless sucessoRanking $
                putStrLn "Erro: Não foi possível atualizar o ranking acumulado."
            return True

-- | Salva a pontuação de um jogador após o fim do jogo
-- Interface principal para registrar o resultado de uma partida.
-- Verifica o estado do jogo e chama registrarResultadoPartida adequadamente.
salvarPontuacao :: String -> Jogo -> IO Bool
salvarPontuacao nomeJogador jogo
  | estadoJogoJogo jogo == Ganhou = registrarResultadoPartida nomeJogador jogo True  -- Jogador ganhou
  | estadoJogoJogo jogo == Perdeu = registrarResultadoPartida nomeJogador jogo False  -- Jogador perdeu
  | otherwise = return True  -- O jogo ainda está em andamento, não salva pontuação

-- | Atualiza o ranking acumulado com a nova pontuação de um jogador
-- Lê o ranking atual, atualiza a pontuação do jogador e salva de volta no arquivo.
atualizarRankingSimplesAcumulado :: String -> Int -> IO Bool
atualizarRankingSimplesAcumulado nomeJogador novaPontuacao = do
    garantirArquivoExiste arquivoRankingAcumulado
    -- Lê o ranking atual
    ranking <- lerRankingAcumulado
    -- Atualiza a pontuação do jogador
    let rankingAtualizado = atualizarPontuacaoJogador nomeJogador novaPontuacao ranking
    -- Formata o conteúdo para o arquivo
    let conteudo = unlines [nome ++ " " ++ show pontos | (nome, pontos) <- rankingAtualizado]
    -- Salva o ranking atualizado
    resultado <- escreverArquivoSeguro arquivoRankingAcumulado conteudo
    return resultado

-- | Exibe o ranking geral ordenado por pontuação
-- Mostra a classificação de todos os jogadores, do maior pontuador para o menor.
exibirRankingGeral :: IO ()
exibirRankingGeral = do
    putStrLn "\n--- Ranking Geral de Jogadores ---"
    rankingCompleto <- lerRankingAcumulado
    if null rankingCompleto
        then putStrLn "Nenhuma pontuação registrada ainda."
        else do
            -- Ordena o ranking por pontuação (ordem decrescente)
            let rankingOrdenado = sortBy (\(_, p1) (_, p2) -> compare p2 p1) rankingCompleto
            -- Imprime cada entrada do ranking com sua posição
            mapM_ imprimirEntradaRanking (zip [1..] rankingOrdenado)
    putStrLn "------------------------------------\n"

-- | Exibe o histórico das partidas mais recentes
-- Mostra detalhes das últimas N partidas jogadas.
exibirHistoricoPartidas :: Int -> IO ()
exibirHistoricoPartidas numPartidas = do
    putStrLn "\n--- Histórico de Partidas Recentes ---"
    historicoCompleto <- lerHistoricoPartidas
    if null historicoCompleto
        then putStrLn "Nenhuma partida registrada ainda."
        else do
            -- Inverte a ordem para mostrar as mais recentes primeiro
            let historicoReverso = reverse historicoCompleto
            -- Limita ao número de partidas solicitado
            let historicoRecente = take numPartidas historicoReverso
            -- Imprime cada partida com seu número
            mapM_ imprimirHistoricoPartida (zip [1..] historicoRecente)
    putStrLn "------------------------------------\n"

-- | Imprime uma entrada do histórico de partidas
-- Formata e exibe os detalhes de uma partida específica.
imprimirHistoricoPartida :: (Int, PontuacaoJogador) -> IO ()
imprimirHistoricoPartida (numero, (nome, pontos, resultado, palavra)) =
    putStrLn $ show numero ++ ". " ++ nome ++ " marcou " ++ show pontos ++ " pontos (" ++ resultado ++ " - palavra: " ++ palavra ++ ")"

-- | Imprime uma entrada do ranking
-- Formata e exibe a pontuação acumulada de um jogador.
imprimirEntradaRanking :: (Int, (String, Int)) -> IO ()
imprimirEntradaRanking (posicao, (nome, pontos)) =
    putStrLn $ show posicao ++ ". " ++ nome ++ " - " ++ show pontos ++ " pontos"

-- | Alias para exibirRankingGeral
-- Mantido por compatibilidade com versões anteriores.
exibirRankingAcumulado :: IO ()
exibirRankingAcumulado = exibirRankingGeral

-- | Alias para exibirHistoricoPartidas
-- Mantido por compatibilidade com versões anteriores.
exibirRanking :: Int -> IO ()
exibirRanking = exibirHistoricoPartidas

-- | Lê o ranking (função de compatibilidade com versões anteriores)
-- Converte o histórico de partidas para o formato de ranking.
lerRanking :: IO [(String, Int)]
lerRanking = do
    historico <- lerHistoricoPartidas
    return [(nome, pontos) | (nome, pontos, _, _) <- historico]

-- | Caminho para o arquivo de ranking (compatibilidade com versões anteriores)
arquivoRanking :: FilePath
arquivoRanking = arquivoHistoricoPartidas