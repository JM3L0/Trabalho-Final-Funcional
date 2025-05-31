-- Ranking.hs
-- | Gerencia o sistema de pontuação e ranking do Jogo da Forca.
module Ranking (
  PontuacaoJogador,
  arquivoHistoricoPartidas,
  arquivoRankingAcumulado,
  lerHistoricoPartidas,
  lerRankingAcumulado,
  salvarPontuacao,
  exibirRankingGeral,
  exibirHistoricoPartidas
) where

import System.IO
import Tipos (Jogo, estadoJogo, EstadoJogo(Ganhou, Perdeu), palavraSecreta)
import LogicaJogo (calcularPontuacao)
import Utilitarios (trim) -- Certifique-se que Utilitarios exporta trim
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)
import Control.Monad (unless)
import Control.Exception (evaluate)

type PontuacaoJogador = (String, Int, String, String)

arquivoHistoricoPartidas :: FilePath
arquivoHistoricoPartidas = "data/historico_partidas.txt"

arquivoRankingAcumulado :: FilePath
arquivoRankingAcumulado = "data/ranking_acumulado.txt"

garantirArquivoExiste :: FilePath -> IO ()
garantirArquivoExiste _arquivo = return ()

lerArquivoSeguro :: FilePath -> IO String
lerArquivoSeguro arquivo = catchIOError strictReadFileHandler handler
  where
    strictReadFileHandler :: IO String
    strictReadFileHandler = do
        conteudo <- readFile arquivo
        _ <- evaluate (length conteudo)
        return conteudo
    handler :: IOError -> IO String
    handler _err = do
        -- Se precisar ver erros de leitura, descomente a linha abaixo:
        -- putStrLn $ "[AVISO Ranking.hs] Não foi possível ler o arquivo '" ++ arquivo ++ "'. Erro: " ++ show _err
        return ""

acrescentarArquivoSeguro :: FilePath -> String -> IO Bool
acrescentarArquivoSeguro arquivo conteudo = do
    resultado <- catchIOError
        (appendFile arquivo conteudo >> return True)
        (\e -> putStrLn ("Erro ao acrescentar ao arquivo: " ++ arquivo ++ " - " ++ show e) >> return False)
    return resultado

escreverArquivoSeguro :: FilePath -> String -> IO Bool
escreverArquivoSeguro arquivo conteudo = do
    resultado <- catchIOError
        (writeFile arquivo conteudo >> return True)
        (\e -> putStrLn ("Erro ao escrever arquivo: " ++ arquivo ++ " - " ++ show e) >> return False)
    return resultado

-- << FUNÇÃO MODIFICADA PARA LIDAR COM NOMES COM ESPAÇOS >>
parseLinhaPontuacao :: String -> Maybe (String, Int)
parseLinhaPontuacao linha =
    let ws = words linha
    in if null ws then Nothing else -- Linha vazia não produz nada
        let pontosStr = last ws
            nomeParts = init ws -- Todas as palavras exceto a última
        in if null nomeParts then Nothing else -- Precisa ter um nome
           case readMaybe pontosStr :: Maybe Int of
                Just pontos -> Just (unwords nomeParts, pontos)
                Nothing -> Nothing -- Última palavra não é um número

-- << FUNÇÃO MODIFICADA PARA LIDAR COM NOMES COM ESPAÇOS >>
parseLinhaHistorico :: String -> Maybe PontuacaoJogador
parseLinhaHistorico linha =
    let ws = words linha
    -- Esperamos pelo menos: Nome (1 palavra) + Pontos (1) + (Resultado (1) + - (1) + palavra: (1) + PALAVRA) (1)) = 6 palavras
    in if length ws < 6 then Nothing else
        let numWords = length ws
            -- Pegando as partes fixas do final da string
            palavraComParenteses = ws !! (numWords - 1)         -- Ex: "CONSTANTE)"
            -- palavraKeyword = ws !! (numWords - 2)            -- Ex: "palavra:" (usado para validação abaixo)
            -- dash = ws !! (numWords - 3)                      -- Ex: "-" (usado para validação abaixo)
            -- resultadoComParentesesOriginal = ws !! (numWords - 4) -- Ex: "(venceu" (usado para validação abaixo)
            pontosStr = ws !! (numWords - 5)                 -- Ex: "65"
            nomeParts = take (numWords - 5) ws               -- Ex: ["pedro"] ou ["joao", "marcos"]

            -- Reconstruindo a parte dos detalhes para usar a lógica de parsing existente
            restoDetalhes = unwords (drop (numWords - 4) ws) -- Ex: "(venceu - palavra: CONSTANTE)"

        in if null nomeParts || (ws !! (numWords - 2)) /= "palavra:" || (ws !! (numWords - 3)) /= "-"
           then Nothing -- Estrutura básica do final não confere
           else case readMaybe pontosStr :: Maybe Int of
                Just pontos ->
                    let nome = unwords nomeParts
                        -- Parse 'resultado' e 'palavraLimpa' de 'restoDetalhes'
                        resultado = if "(venceu" `isPrefixOf` restoDetalhes then "venceu" else "perdeu"
                        strAposPalavraKeyword = snd $ breakSubstring "palavra:" restoDetalhes
                        palavraComLixo = if null strAposPalavraKeyword then "" else drop (length "palavra: ") strAposPalavraKeyword
                        palavraLimpa = trim $ filter (\c -> c /= ')' && c /= '(') palavraComLixo
                    in if not (null palavraLimpa) && -- Garante que a palavra foi encontrada
                         ((resultado == "venceu" && "(venceu" `isPrefixOf` restoDetalhes) || (resultado == "perdeu" && "(perdeu" `isPrefixOf` restoDetalhes))
                        then Just (nome, pontos, resultado, palavraLimpa)
                        else Nothing -- Parsing dos detalhes da string (resultado, palavra) falhou
                Nothing -> Nothing -- String de pontos não era um Int

  where
    -- Helpers locais (trim é importado de Utilitarios)
    isPrefixOf prefix str = take (length prefix) str == prefix
    breakSubstring :: String -> String -> (String, String)
    breakSubstring needle haystack = go [] haystack
      where
        go acc [] = (reverse acc, [])
        go acc src@(c:cs)
          | needle `isPrefixOf` src = (reverse acc, src)
          | otherwise               = go (c:acc) cs

atualizarPontuacaoJogador :: String -> Int -> [(String, Int)] -> [(String, Int)]
atualizarPontuacaoJogador nome novaPontuacao [] = [(nome, novaPontuacao)]
atualizarPontuacaoJogador nome novaPontuacao ((jogador, pontos):resto)
    | jogador == nome = (jogador, pontos + novaPontuacao) : resto
    | otherwise = (jogador, pontos) : atualizarPontuacaoJogador nome novaPontuacao resto

lerHistoricoPartidas :: IO [PontuacaoJogador]
lerHistoricoPartidas = do
    garantirArquivoExiste arquivoHistoricoPartidas
    conteudo <- lerArquivoSeguro arquivoHistoricoPartidas
    let linhas = lines conteudo
    return (mapMaybe parseLinhaHistorico linhas)

lerRankingAcumulado :: IO [(String, Int)]
lerRankingAcumulado = do
    garantirArquivoExiste arquivoRankingAcumulado
    conteudo <- lerArquivoSeguro arquivoRankingAcumulado
    let linhas = lines conteudo
    return (mapMaybe parseLinhaPontuacao linhas)

registrarResultadoPartida :: String -> Jogo -> Bool -> IO Bool
registrarResultadoPartida nomeJogador jogo ganhou = do
    garantirArquivoExiste arquivoHistoricoPartidas
    let pontuacao = if ganhou then calcularPontuacao jogo else 0
    let resultado = if ganhou then "venceu" else "perdeu"
    let linha = nomeJogador ++ " " ++ show pontuacao ++ " (" ++ resultado ++ " - palavra: " ++ palavraSecreta jogo ++ ")\n"
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

salvarPontuacao :: String -> Jogo -> IO Bool
salvarPontuacao nomeJogador jogo
  | estadoJogo jogo == Ganhou = registrarResultadoPartida nomeJogador jogo True
  | estadoJogo jogo == Perdeu = registrarResultadoPartida nomeJogador jogo False
  | otherwise = return True

atualizarRankingSimplesAcumulado :: String -> Int -> IO Bool
atualizarRankingSimplesAcumulado nomeJogador novaPontuacao = do
    garantirArquivoExiste arquivoRankingAcumulado
    ranking <- lerRankingAcumulado
    let rankingAtualizado = atualizarPontuacaoJogador nomeJogador novaPontuacao ranking
    let conteudo = unlines [nome ++ " " ++ show pontos | (nome, pontos) <- rankingAtualizado]
    resultado <- escreverArquivoSeguro arquivoRankingAcumulado conteudo
    return resultado

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

imprimirHistoricoPartida :: (Int, PontuacaoJogador) -> IO ()
imprimirHistoricoPartida (numero, (nome, pontos, resultado, palavra)) =
    putStrLn $ show numero ++ ". " ++ nome ++ " marcou " ++ show pontos ++ " pontos (" ++ resultado ++ " - palavra: " ++ palavra ++ ")"

imprimirEntradaRanking :: (Int, (String, Int)) -> IO ()
imprimirEntradaRanking (posicao, (nome, pontos)) =
    putStrLn $ show posicao ++ ". " ++ nome ++ " - " ++ show pontos ++ " pontos"

exibirRankingAcumulado :: IO ()
exibirRankingAcumulado = exibirRankingGeral

exibirRanking :: Int -> IO ()
exibirRanking = exibirHistoricoPartidas

lerRanking :: IO [(String, Int)]
lerRanking = do
    historico <- lerHistoricoPartidas
    return [(nome, pontos) | (nome, pontos, _, _) <- historico]

arquivoRanking :: FilePath
arquivoRanking = arquivoHistoricoPartidas