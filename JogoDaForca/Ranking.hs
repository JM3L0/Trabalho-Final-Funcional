-- Ranking.hs
-- | Gerencia o sistema de pontuação e ranking do Jogo da Forca.
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
type PontuacaoJogador = (String, Int, String, String)

-- | Caminho para o arquivo de histórico de partidas
arquivoHistoricoPartidas :: FilePath
arquivoHistoricoPartidas = "data/historico_partidas.txt"

-- | Caminho para o arquivo de ranking acumulado
arquivoRankingAcumulado :: FilePath
arquivoRankingAcumulado = "data/ranking_acumulado.txt"

-- | Garante que um arquivo existe antes de tentar ler/escrever nele
garantirArquivoExiste :: FilePath -> IO ()
garantirArquivoExiste _arquivo = return () -- Implementação vazia, poderia criar o arquivo se necessário

-- | Lê um arquivo de forma segura, capturando possíveis erros
lerArquivoSeguro :: FilePath -> IO String
lerArquivoSeguro arquivo = catchIOError strictReadFileHandler handler
  where
    -- Força a avaliação do conteúdo para garantir que erros de leitura sejam detectados imediatamente
    strictReadFileHandler :: IO String
    strictReadFileHandler = do
        conteudo <- readFile arquivo
        _ <- evaluate (length conteudo)
        return conteudo
    -- Manipulador de erros que retorna uma string vazia em caso de erro
    handler :: IOError -> IO String
    handler _err = do
        -- putStrLn $ "[AVISO Ranking.hs] Não foi possível ler o arquivo '" ++ arquivo ++ "'. Erro: " ++ show _err
        return ""

-- | Acrescenta conteúdo a um arquivo de forma segura
acrescentarArquivoSeguro :: FilePath -> String -> IO Bool
acrescentarArquivoSeguro arquivo conteudo = do
    resultado <- catchIOError
        (appendFile arquivo conteudo >> return True)
        (\e -> putStrLn ("Erro ao acrescentar ao arquivo: " ++ arquivo ++ " - " ++ show e) >> return False)
    return resultado

-- | Escreve conteúdo em um arquivo de forma segura
escreverArquivoSeguro :: FilePath -> String -> IO Bool
escreverArquivoSeguro arquivo conteudo = do
    resultado <- catchIOError
        (writeFile arquivo conteudo >> return True)
        (\e -> putStrLn ("Erro ao escrever arquivo: " ++ arquivo ++ " - " ++ show e) >> return False)
    return resultado

-- | Analisa uma linha do arquivo de pontuação e retorna um par (nome, pontos)
parseLinhaPontuacao :: String -> Maybe (String, Int)
parseLinhaPontuacao linha =
    let ws = words linha
    in if null ws then Nothing else
        let pontosStr = last ws
            nomeParts = init ws
        in if null nomeParts then Nothing else
           case readMaybe pontosStr :: Maybe Int of
                Just pontos -> Just (unwords nomeParts, pontos)
                Nothing -> Nothing

-- | Analisa uma linha do arquivo de histórico e retorna uma PontuacaoJogador
parseLinhaHistorico :: String -> Maybe PontuacaoJogador
parseLinhaHistorico linha =
    let ws = words linha
    in if length ws < 6 then Nothing else
        let numWords = length ws
            palavraComParenteses = ws !! (numWords - 1)
            pontosStr = ws !! (numWords - 5)
            nomeParts = take (numWords - 5) ws
            restoDetalhes = unwords (drop (numWords - 4) ws)
        in if null nomeParts || (ws !! (numWords - 2)) /= "palavra:" || (ws !! (numWords - 3)) /= "-"
           then Nothing
           else case readMaybe pontosStr :: Maybe Int of
                Just pontos ->
                    let nome = unwords nomeParts
                        resultado = if "(venceu" `isPrefixOf` restoDetalhes then "venceu" else "perdeu"
                        strAposPalavraKeyword = snd $ breakSubstring "palavra:" restoDetalhes
                        palavraComLixo = if null strAposPalavraKeyword then "" else drop (length "palavra: ") strAposPalavraKeyword
                        palavraLimpa = trim $ filter (\c -> c /= ')' && c /= '(') palavraComLixo
                    in if not (null palavraLimpa) &&
                         ((resultado == "venceu" && "(venceu" `isPrefixOf` restoDetalhes) || (resultado == "perdeu" && "(perdeu" `isPrefixOf` restoDetalhes))
                        then Just (nome, pontos, resultado, palavraLimpa)
                        else Nothing
                Nothing -> Nothing
  where
    -- Verifica se uma string é prefixo de outra
    isPrefixOf prefix str = take (length prefix) str == prefix
    -- Divide uma string em duas partes: antes e depois da primeira ocorrência de uma substring
    breakSubstring :: String -> String -> (String, String)
    breakSubstring needle haystack = go [] haystack
      where
        go acc [] = (reverse acc, [])
        go acc src@(c:cs)
          | needle `isPrefixOf` src = (reverse acc, src)
          | otherwise               = go (c:acc) cs

-- | Atualiza a pontuação de um jogador no ranking
atualizarPontuacaoJogador :: String -> Int -> [(String, Int)] -> [(String, Int)]
atualizarPontuacaoJogador nome novaPontuacao [] = [(nome, novaPontuacao)]
atualizarPontuacaoJogador nome novaPontuacao ((jogador, pontos):resto)
    | jogador == nome = (jogador, pontos + novaPontuacao) : resto  -- Atualiza pontuação do jogador existente
    | otherwise = (jogador, pontos) : atualizarPontuacaoJogador nome novaPontuacao resto  -- Continua procurando

-- | Lê o histórico de partidas do arquivo
lerHistoricoPartidas :: IO [PontuacaoJogador]
lerHistoricoPartidas = do
    garantirArquivoExiste arquivoHistoricoPartidas
    conteudo <- lerArquivoSeguro arquivoHistoricoPartidas
    let linhas = lines conteudo
    return (mapMaybe parseLinhaHistorico linhas)

-- | Lê o ranking acumulado do arquivo
lerRankingAcumulado :: IO [(String, Int)]
lerRankingAcumulado = do
    garantirArquivoExiste arquivoRankingAcumulado
    conteudo <- lerArquivoSeguro arquivoRankingAcumulado
    let linhas = lines conteudo
    return (mapMaybe parseLinhaPontuacao linhas)

-- | Registra o resultado de uma partida no histórico e atualiza o ranking
registrarResultadoPartida :: String -> Jogo -> Bool -> IO Bool
registrarResultadoPartida nomeJogador jogo ganhou = do
    garantirArquivoExiste arquivoHistoricoPartidas
    let pontuacao = if ganhou then calcularPontuacao jogo else 0
    let resultado = if ganhou then "venceu" else "perdeu"
    let linha = nomeJogador ++ " " ++ show pontuacao ++ " (" ++ resultado ++ " - palavra: " ++ palavraSecretaJogo jogo ++ ")\n"
    
    sucessoHistorico <- acrescentarArquivoSeguro arquivoHistoricoPartidas linha
    
    if not sucessoHistorico
        then do
            putStrLn "Erro: Não foi possível salvar no histórico de partidas."
            return False
        else do
            sucessoRanking <- atualizarRankingSimplesAcumulado nomeJogador pontuacao
            unless sucessoRanking $
                putStrLn "Erro: Não foi possível atualizar o ranking acumulado."
            return True

-- | Salva a pontuação de um jogador após o fim do jogo
salvarPontuacao :: String -> Jogo -> IO Bool
salvarPontuacao nomeJogador jogo
  | estadoJogoJogo jogo == Ganhou = registrarResultadoPartida nomeJogador jogo True
  | estadoJogoJogo jogo == Perdeu = registrarResultadoPartida nomeJogador jogo False
  | otherwise = return True  -- O jogo ainda está em andamento, não salva pontuação

-- | Atualiza o ranking acumulado com a nova pontuação de um jogador
atualizarRankingSimplesAcumulado :: String -> Int -> IO Bool
atualizarRankingSimplesAcumulado nomeJogador novaPontuacao = do
    garantirArquivoExiste arquivoRankingAcumulado
    ranking <- lerRankingAcumulado
    let rankingAtualizado = atualizarPontuacaoJogador nomeJogador novaPontuacao ranking
    let conteudo = unlines [nome ++ " " ++ show pontos | (nome, pontos) <- rankingAtualizado]
    resultado <- escreverArquivoSeguro arquivoRankingAcumulado conteudo
    return resultado

-- | Exibe o ranking geral ordenado por pontuação
exibirRankingGeral :: IO ()
exibirRankingGeral = do
    putStrLn "\n--- Ranking Geral de Jogadores ---"
    rankingCompleto <- lerRankingAcumulado
    if null rankingCompleto
        then putStrLn "Nenhuma pontuação registrada ainda."
        else do
            let rankingOrdenado = sortBy (\(_, p1) (_, p2) -> compare p2 p1) rankingCompleto
            mapM_ imprimirEntradaRanking (zip [1..] rankingOrdenado)
    putStrLn "------------------------------------\n"

-- | Exibe o histórico das partidas mais recentes
exibirHistoricoPartidas :: Int -> IO ()
exibirHistoricoPartidas numPartidas = do
    putStrLn "\n--- Histórico de Partidas Recentes ---"
    historicoCompleto <- lerHistoricoPartidas
    if null historicoCompleto
        then putStrLn "Nenhuma partida registrada ainda."
        else do
            let historicoReverso = reverse historicoCompleto  -- Mais recentes primeiro
            let historicoRecente = take numPartidas historicoReverso
            mapM_ imprimirHistoricoPartida (zip [1..] historicoRecente)
    putStrLn "------------------------------------\n"

-- | Imprime uma entrada do histórico de partidas
imprimirHistoricoPartida :: (Int, PontuacaoJogador) -> IO ()
imprimirHistoricoPartida (numero, (nome, pontos, resultado, palavra)) =
    putStrLn $ show numero ++ ". " ++ nome ++ " marcou " ++ show pontos ++ " pontos (" ++ resultado ++ " - palavra: " ++ palavra ++ ")"

-- | Imprime uma entrada do ranking
imprimirEntradaRanking :: (Int, (String, Int)) -> IO ()
imprimirEntradaRanking (posicao, (nome, pontos)) =
    putStrLn $ show posicao ++ ". " ++ nome ++ " - " ++ show pontos ++ " pontos"

-- | Alias para exibirRankingGeral
exibirRankingAcumulado :: IO ()
exibirRankingAcumulado = exibirRankingGeral

-- | Alias para exibirHistoricoPartidas
exibirRanking :: Int -> IO ()
exibirRanking = exibirHistoricoPartidas

-- | Lê o ranking (função de compatibilidade com versões anteriores)
lerRanking :: IO [(String, Int)]
lerRanking = do
    historico <- lerHistoricoPartidas
    return [(nome, pontos) | (nome, pontos, _, _) <- historico]

-- | Caminho para o arquivo de ranking (compatibilidade com versões anteriores)
arquivoRanking :: FilePath
arquivoRanking = arquivoHistoricoPartidas