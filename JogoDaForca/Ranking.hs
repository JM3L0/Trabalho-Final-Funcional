-- | Gerencia o sistema de pontuação e ranking do Jogo da Forca.
-- Este módulo lida com a persistência de dados (leitura e escrita em arquivo),
-- tratamento de erros de IO e manipulação de listas para ordenação.
module Ranking (
  -- * Tipos e Constantes
  PontuacaoJogador,     -- ^ Alias de tipo para representar (Nome, Pontos, Resultado, Palavra).
  arquivoHistoricoPartidas, -- ^ Caminho do arquivo de histórico de partidas.
  arquivoRankingAcumulado, -- ^ Caminho do arquivo de ranking acumulado.
  -- * Funções de IO
  lerHistoricoPartidas,  -- ^ Lê o histórico de partidas do arquivo.
  lerRankingAcumulado,  -- ^ Lê o ranking acumulado do arquivo.
  salvarPontuacao,      -- ^ Salva a pontuação de um jogador nos arquivos.
  exibirRankingGeral,   -- << ALTERADO: de exibirRankingMelhores para exibirRankingGeral
  exibirHistoricoPartidas -- ^ Exibe o histórico das partidas recentes.
) where

import System.IO
import Tipos (Jogo, estadoJogo, EstadoJogo(Ganhou, Perdeu), palavraSecreta)
import LogicaJogo (calcularPontuacao)
import Utilitarios (trim)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)
import Control.Monad (unless) -- Removido 'when' pois a lógica mudou
import Control.Exception (evaluate)

-- | Representa a pontuação de um jogador como uma tupla (Nome, Pontos, Resultado, Palavra).
type PontuacaoJogador = (String, Int, String, String)

-- | Caminho para o arquivo de histórico - na pasta data/
arquivoHistoricoPartidas :: FilePath
arquivoHistoricoPartidas = "data/historico_partidas.txt"

-- | Caminho para o arquivo de ranking acumulado - na pasta data/
arquivoRankingAcumulado :: FilePath
arquivoRankingAcumulado = "data/ranking_acumulado.txt"

-- | Função para garantir que o arquivo exista (simplificada).
garantirArquivoExiste :: FilePath -> IO ()
garantirArquivoExiste _arquivo = do
    return ()

-- | Lê o conteúdo de um arquivo com tratamento simplificado de erros e de forma estrita.
lerArquivoSeguro :: FilePath -> IO String
lerArquivoSeguro arquivo = catchIOError strictReadFileHandler handler
  where
    strictReadFileHandler :: IO String
    strictReadFileHandler = do
        conteudo <- readFile arquivo
        _ <- evaluate (length conteudo)
        return conteudo
    handler :: IOError -> IO String
    handler _err = return ""

-- | Acrescenta ao final de um arquivo usando appendFile.
acrescentarArquivoSeguro :: FilePath -> String -> IO Bool
acrescentarArquivoSeguro arquivo conteudo = do
    resultado <- catchIOError
        (do
            appendFile arquivo conteudo
            return True
        )
        (\e -> do
            putStrLn $ "Erro ao acrescentar ao arquivo: " ++ arquivo ++ " - " ++ show e
            return False
        )
    return resultado

-- | Escreve em um arquivo (sobrescrevendo) usando writeFile.
escreverArquivoSeguro :: FilePath -> String -> IO Bool
escreverArquivoSeguro arquivo conteudo = do
    resultado <- catchIOError
        (do
            writeFile arquivo conteudo
            return True
        )
        (\e -> do
            putStrLn $ "Erro ao escrever arquivo: " ++ arquivo ++ " - " ++ show e
            return False
        )
    return resultado

-- | Tenta converter uma linha do arquivo de ranking "Nome Pontos" em uma tupla (Nome, Pontos).
parseLinhaPontuacao :: String -> Maybe (String, Int)
parseLinhaPontuacao linha = case words linha of
    (nome:pontosStr:_) -> case readMaybe pontosStr :: Maybe Int of
                            Just pontos -> Just (nome, pontos)
                            Nothing -> Nothing
    _ -> Nothing

-- | Tenta converter uma linha do arquivo de histórico.
parseLinhaHistorico :: String -> Maybe PontuacaoJogador
parseLinhaHistorico linha =
    let partes = words linha
    in case partes of
        (nome:pontosStr:resto) -> case readMaybe pontosStr :: Maybe Int of
            Just pontos -> do
                let textoCompletoResto = unwords resto
                let resultado = if "(venceu" `isPrefixOf` textoCompletoResto then "venceu" else "perdeu"
                let strAposPalavraKeyword = snd $ breakSubstring "palavra:" textoCompletoResto
                let palavraComLixo = if null strAposPalavraKeyword then "" else drop (length "palavra: ") strAposPalavraKeyword
                let palavraLimpa = trim $ filter (\c -> c /= ')' && c /= '(') palavraComLixo
                Just (nome, pontos, resultado, palavraLimpa)
            Nothing -> Nothing
        _ -> Nothing
  where
    isPrefixOf prefix str = take (length prefix) str == prefix
    breakSubstring :: String -> String -> (String, String)
    breakSubstring needle haystack = go [] haystack
      where
        go acc [] = (reverse acc, [])
        go acc src@(c:cs)
          | needle `isPrefixOf` src = (reverse acc, src)
          | otherwise               = go (c:acc) cs

-- | Atualiza a pontuação de um jogador na lista de pontuações.
atualizarPontuacaoJogador :: String -> Int -> [(String, Int)] -> [(String, Int)]
atualizarPontuacaoJogador nome novaPontuacao [] = [(nome, novaPontuacao)]
atualizarPontuacaoJogador nome novaPontuacao ((jogador, pontos):resto)
    | jogador == nome = (jogador, pontos + novaPontuacao) : resto
    | otherwise = (jogador, pontos) : atualizarPontuacaoJogador nome novaPontuacao resto

-- | Lê as pontuações do arquivo de histórico de partidas.
lerHistoricoPartidas :: IO [PontuacaoJogador]
lerHistoricoPartidas = do
    garantirArquivoExiste arquivoHistoricoPartidas
    conteudo <- lerArquivoSeguro arquivoHistoricoPartidas
    let linhas = lines conteudo
    return (mapMaybe parseLinhaHistorico linhas)

-- | Lê as pontuações do arquivo de ranking acumulado.
lerRankingAcumulado :: IO [(String, Int)]
lerRankingAcumulado = do
    garantirArquivoExiste arquivoRankingAcumulado
    conteudo <- lerArquivoSeguro arquivoRankingAcumulado
    let linhas = lines conteudo
    return (mapMaybe parseLinhaPontuacao linhas)

-- | Registra um resultado de partida (ganhou ou perdeu).
registrarResultadoPartida :: String -> Jogo -> Bool -> IO Bool
registrarResultadoPartida nomeJogador jogo ganhou = do
    garantirArquivoExiste arquivoHistoricoPartidas
    let pontuacao = if ganhou then calcularPontuacao jogo else 0 -- pontuação do jogo atual (0 se perdeu)
    let resultado = if ganhou then "venceu" else "perdeu"
    let linha = nomeJogador ++ " " ++ show pontuacao ++ " (" ++ resultado ++ " - palavra: " ++ palavraSecreta jogo ++ ")\n"
    
    sucessoHistorico <- acrescentarArquivoSeguro arquivoHistoricoPartidas linha
    
    if not sucessoHistorico
        then do
            putStrLn "Erro: Não foi possível salvar no histórico de partidas."
            return False
        else do
            -- << ALTERADO: Tenta atualizar o ranking acumulado para TODOS os jogadores,
            --    independentemente da pontuação ou se ganhou/perdeu.
            --    A 'pontuacao' aqui é a do jogo atual (0 se perdeu).
            --    'atualizarRankingSimplesAcumulado' irá somar essa pontuação ao total do jogador.
            sucessoRanking <- atualizarRankingSimplesAcumulado nomeJogador pontuacao
            unless sucessoRanking $
                putStrLn "Erro: Não foi possível atualizar o ranking acumulado."
            return True -- Retorna True se o histórico foi salvo, mesmo que o ranking acumulado falhe (para não impedir o fluxo principal)

-- | Salva a pontuação de um jogador.
salvarPontuacao :: String -> Jogo -> IO Bool
salvarPontuacao nomeJogador jogo
  | estadoJogo jogo == Ganhou = registrarResultadoPartida nomeJogador jogo True
  | estadoJogo jogo == Perdeu = registrarResultadoPartida nomeJogador jogo False
  | otherwise = return True

-- | Atualiza o ranking acumulado.
atualizarRankingSimplesAcumulado :: String -> Int -> IO Bool
atualizarRankingSimplesAcumulado nomeJogador novaPontuacao = do
    garantirArquivoExiste arquivoRankingAcumulado
    ranking <- lerRankingAcumulado
    let rankingAtualizado = atualizarPontuacaoJogador nomeJogador novaPontuacao ranking
    let conteudo = unlines [nome ++ " " ++ show pontos | (nome, pontos) <- rankingAtualizado]
    resultado <- escreverArquivoSeguro arquivoRankingAcumulado conteudo
    return resultado

-- | Exibe o ranking geral de todos os jogadores.
-- << RENOMEADA e ALTERADA: de exibirRankingMelhores para exibirRankingGeral
exibirRankingGeral :: IO () -- << REMOVIDO parâmetro topN
exibirRankingGeral = do
    putStrLn "\n--- Ranking Geral de Jogadores ---" -- << ALTERADO Título
    rankingCompleto <- lerRankingAcumulado
    if null rankingCompleto
        then putStrLn "Nenhuma pontuação registrada ainda."
        else do
            let rankingOrdenado = sortBy (\(_, p1) (_, p2) -> compare p2 p1) rankingCompleto
            -- << REMOVIDO: let rankingTop = take topN rankingOrdenado
            mapM_ imprimirEntradaRanking (zip [1..] rankingOrdenado) -- << ALTERADO: usa rankingOrdenado
    putStrLn "------------------------------------\n"

-- | Exibe o histórico das partidas mais recentes.
exibirHistoricoPartidas :: Int -> IO ()
exibirHistoricoPartidas numPartidas = do
    putStrLn "\n--- Histórico de Partidas Recentes ---"
    historicoCompleto <- lerHistoricoPartidas
    if null historicoCompleto
        then putStrLn "Nenhuma partida registrada ainda."
        else do
            let historicoReverso = reverse historicoCompleto
            let historicoRecente = take numPartidas historicoReverso
            mapM_ imprimirHistoricoPartida (zip [1..] historicoRecente)
    putStrLn "------------------------------------\n"

-- | Imprime uma entrada do histórico de partidas.
imprimirHistoricoPartida :: (Int, PontuacaoJogador) -> IO ()
imprimirHistoricoPartida (numero, (nome, pontos, resultado, palavra)) = do
    putStrLn $ show numero ++ ". " ++ nome ++ " marcou " ++ show pontos ++ " pontos (" ++ resultado ++ " - palavra: " ++ palavra ++ ")"

-- | Imprime uma entrada formatada do ranking.
imprimirEntradaRanking :: (Int, (String, Int)) -> IO ()
imprimirEntradaRanking (posicao, (nome, pontos)) = do
    putStrLn $ show posicao ++ ". " ++ nome ++ " - " ++ show pontos ++ " pontos"

-- | Funções obsoletas mantidas por compatibilidade
exibirRankingAcumulado :: IO () -- << ALTERADO: Assinatura e implementação
exibirRankingAcumulado = exibirRankingGeral

exibirRanking :: Int -> IO ()
exibirRanking = exibirHistoricoPartidas

-- Corrigido para converter o tipo PontuacaoJogador para (String, Int)
lerRanking :: IO [(String, Int)]
lerRanking = do
    historico <- lerHistoricoPartidas
    return [(nome, pontos) | (nome, pontos, _, _) <- historico]

arquivoRanking :: FilePath
arquivoRanking = arquivoHistoricoPartidas