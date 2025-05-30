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
  exibirRankingMelhores, -- ^ Exibe o ranking dos melhores jogadores (acumulado).
  exibirHistoricoPartidas -- ^ Exibe o histórico das partidas recentes.
) where
    
import System.IO
import Tipos (Jogo, estadoJogo, EstadoJogo(Ganhou, Perdeu), palavraSecreta)
import LogicaJogo (calcularPontuacao)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import System.IO (readFile, writeFile, appendFile, hClose, openFile, IOMode(AppendMode, WriteMode, ReadMode))
import System.IO.Error (catchIOError, isDoesNotExistError)
import Text.Read (readMaybe)
import Control.Exception (try, catch, SomeException)
import Control.Monad (when, unless)

-- | Representa a pontuação de um jogador como uma tupla (Nome, Pontos, Resultado, Palavra).
type PontuacaoJogador = (String, Int, String, String)

-- | Caminho para o arquivo de histórico - na pasta data/
arquivoHistoricoPartidas :: FilePath
arquivoHistoricoPartidas = "data/historico_partidas.txt"

-- | Caminho para o arquivo de ranking acumulado - na pasta data/
arquivoRankingAcumulado :: FilePath
arquivoRankingAcumulado = "data/ranking_acumulado.txt"

-- | Função para garantir que o arquivo exista (versão robusta)
garantirArquivoExiste :: FilePath -> IO ()
garantirArquivoExiste arquivo = do
    existe <- catchIOError (readFile arquivo >> return True) (\_ -> return False)
    unless existe $ do
        putStrLn $ "Criando arquivo: " ++ arquivo
        catchIOError (writeFile arquivo "") (\e -> 
            putStrLn $ "Aviso: Não foi possível criar " ++ arquivo ++ ": " ++ show e)
  where
    unless cond action = if not cond then action else return ()

-- | Lê o conteúdo de um arquivo com tratamento simplificado de erros
lerArquivoSeguro :: FilePath -> IO String
lerArquivoSeguro arquivo = catchIOError (readFile arquivo) (\_ -> return "")

-- | Acrescenta ao final de um arquivo com técnica anti-bloqueio
acrescentarArquivoSeguro :: FilePath -> String -> IO Bool
acrescentarArquivoSeguro arquivo conteudo = do
    putStrLn $ "Tentando salvar em: " ++ arquivo
    
    -- Primeiro, tentar ler o conteúdo atual (ou usar string vazia se falhar)
    conteudoAtual <- catchIOError 
        (readFile arquivo) 
        (\_ -> return "")
    
    -- Esperar um momento para garantir que o arquivo seja liberado após a leitura
    esperar 100
    
    -- Escrever todo o conteúdo de volta com a nova linha adicionada
    resultado <- catchIOError 
        (do
            writeFile arquivo (conteudoAtual ++ conteudo)
            return True
        ) 
        (\e -> do
            putStrLn $ "Erro ao escrever arquivo: " ++ arquivo ++ " - " ++ show e
            -- Tenta mais uma vez após esperar
            esperar 200
            segundaTentativa <- catchIOError 
                (writeFile arquivo (conteudoAtual ++ conteudo) >> return True)
                (\_ -> return False)
            return segundaTentativa
        )
    
    if resultado
        then putStrLn $ "Dados salvos com sucesso em: " ++ arquivo
        else putStrLn $ "Falha ao salvar dados em: " ++ arquivo
    
    return resultado
  where
    -- Função auxiliar para esperar um determinado número de milissegundos
    esperar :: Int -> IO ()
    esperar ms = do
        let loopVazio 0 = return ()
            loopVazio n = return () >> loopVazio (n-1)
        loopVazio (ms * 1000)  -- Implementação simples sem depender de bibliotecas

-- | Escreve em um arquivo com técnica anti-bloqueio
escreverArquivoSeguro :: FilePath -> String -> IO Bool
escreverArquivoSeguro arquivo conteudo = do
    putStrLn $ "Tentando escrever em: " ++ arquivo
    
    -- Esperar um momento antes de tentar escrever
    esperar 100
    
    resultado <- catchIOError 
        (do
            writeFile arquivo conteudo
            return True
        ) 
        (\e -> do
            putStrLn $ "Erro ao escrever arquivo: " ++ arquivo ++ " - " ++ show e
            -- Tenta mais uma vez após esperar
            esperar 200
            segundaTentativa <- catchIOError 
                (writeFile arquivo conteudo >> return True)
                (\_ -> return False)
            return segundaTentativa
        )
    
    if resultado
        then putStrLn $ "Dados salvos com sucesso em: " ++ arquivo
        else putStrLn $ "Falha ao salvar dados em: " ++ arquivo
    
    return resultado
  where
    -- Função auxiliar para esperar um determinado número de milissegundos
    esperar :: Int -> IO ()
    esperar ms = do
        let loopVazio 0 = return ()
            loopVazio n = return () >> loopVazio (n-1)
        loopVazio (ms * 1000)  -- Implementação simples sem depender de bibliotecas

-- | Tenta converter uma linha do arquivo de ranking "Nome Pontos" em uma tupla (Nome, Pontos).
parseLinhaPontuacao :: String -> Maybe (String, Int)
parseLinhaPontuacao linha = case words linha of
    (nome:pontosStr:_) -> case readMaybe pontosStr :: Maybe Int of
                            Just pontos -> Just (nome, pontos)
                            Nothing -> Nothing
    _ -> Nothing

-- | Tenta converter uma linha do arquivo de histórico "Nome Pontos (resultado - palavra: PALAVRA)" em uma `PontuacaoJogador`.
parseLinhaHistorico :: String -> Maybe PontuacaoJogador
parseLinhaHistorico linha = 
    let partes = words linha
    in case partes of
        (nome:pontosStr:resto) -> case readMaybe pontosStr :: Maybe Int of
            Just pontos -> do
                let resultado = if "(venceu" `isPrefixOf` unwords resto then "venceu" else "perdeu"
                let palavra = last (splitOn "palavra:" (unwords resto))
                let palavraLimpa = filter (\c -> c /= ')' && c /= '(') palavra
                Just (nome, pontos, resultado, palavraLimpa)
            Nothing -> Nothing
        _ -> Nothing
  where
    isPrefixOf prefix str = take (length prefix) str == prefix
    splitOn delim str = let (before, after) = breakOn delim str in [before, after]
    breakOn delim str = let (b, a) = break (== head delim) str in (b, drop (length delim) a)

-- | Atualiza a pontuação de um jogador na lista de pontuações.
atualizarPontuacaoJogador :: String -> Int -> [(String, Int)] -> [(String, Int)]
atualizarPontuacaoJogador nome novaPontuacao [] = [(nome, novaPontuacao)]
atualizarPontuacaoJogador nome novaPontuacao ((jogador, pontos):resto)
    | jogador == nome = (jogador, pontos + novaPontuacao) : resto
    | otherwise = (jogador, pontos) : atualizarPontuacaoJogador nome novaPontuacao resto

-- | Lê as pontuações do arquivo de histórico de partidas
lerHistoricoPartidas :: IO [PontuacaoJogador]
lerHistoricoPartidas = do
    garantirArquivoExiste arquivoHistoricoPartidas
    conteudo <- lerArquivoSeguro arquivoHistoricoPartidas
    let linhas = lines conteudo
    return (mapMaybe parseLinhaHistorico linhas)

-- | Lê as pontuações do arquivo de ranking acumulado
lerRankingAcumulado :: IO [(String, Int)]
lerRankingAcumulado = do
    garantirArquivoExiste arquivoRankingAcumulado
    conteudo <- lerArquivoSeguro arquivoRankingAcumulado
    let linhas = lines conteudo
    return (mapMaybe parseLinhaPontuacao linhas)

-- | Registra um resultado de partida (ganhou ou perdeu)
registrarResultadoPartida :: String -> Jogo -> Bool -> IO Bool
registrarResultadoPartida nomeJogador jogo ganhou = do
    garantirArquivoExiste arquivoHistoricoPartidas
    let pontuacao = if ganhou then calcularPontuacao jogo else 0
    let resultado = if ganhou then "venceu" else "perdeu"
    let linha = nomeJogador ++ " " ++ show pontuacao ++ " (" ++ resultado ++ " - palavra: " ++ palavraSecreta jogo ++ ")\n"
    putStrLn $ "Tentando salvar no histórico: " ++ linha
    sucesso <- acrescentarArquivoSeguro arquivoHistoricoPartidas linha
    if not sucesso
        then do
            putStrLn "Erro: Não foi possível salvar no histórico de partidas."
            return False
        else do
            when (ganhou && pontuacao > 0) $ do
                sucessoRanking <- atualizarRankingSimplesAcumulado nomeJogador pontuacao
                unless sucessoRanking $
                    putStrLn "Erro: Não foi possível atualizar o ranking acumulado."
            return True

-- | Salva a pontuação de um jogador
salvarPontuacao :: String -> Jogo -> IO Bool
salvarPontuacao nomeJogador jogo
  | estadoJogo jogo == Ganhou = registrarResultadoPartida nomeJogador jogo True
  | estadoJogo jogo == Perdeu = registrarResultadoPartida nomeJogador jogo False
  | otherwise = return True

-- | Atualiza o ranking acumulado (versão simplificada)
atualizarRankingSimplesAcumulado :: String -> Int -> IO Bool
atualizarRankingSimplesAcumulado nomeJogador novaPontuacao = do
    garantirArquivoExiste arquivoRankingAcumulado
    ranking <- lerRankingAcumulado
    let rankingAtualizado = atualizarPontuacaoJogador nomeJogador novaPontuacao ranking
    let conteudo = unlines [nome ++ " " ++ show pontos | (nome, pontos) <- rankingAtualizado]
    putStrLn $ "Tentando salvar ranking acumulado: " ++ conteudo
    escreverArquivoSeguro arquivoRankingAcumulado conteudo

-- | Exibe o ranking dos melhores jogadores
exibirRankingMelhores :: Int -> IO ()
exibirRankingMelhores topN = do
    putStrLn "\n--- Ranking dos Melhores Jogadores ---"
    rankingCompleto <- lerRankingAcumulado
    if null rankingCompleto
        then putStrLn "Nenhuma pontuação registrada ainda."
        else do
            let rankingOrdenado = sortBy (\(_, p1) (_, p2) -> compare p2 p1) rankingCompleto
            let rankingTop = take topN rankingOrdenado
            mapM_ imprimirEntradaRanking (zip [1..] rankingTop)
    putStrLn "------------------------------------\n"

-- | Exibe o histórico das partidas mais recentes
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

-- | Imprime uma entrada formatada do ranking
imprimirEntradaRanking :: (Int, (String, Int)) -> IO ()
imprimirEntradaRanking (posicao, (nome, pontos)) = do
    putStrLn $ show posicao ++ ". " ++ nome ++ " - " ++ show pontos ++ " pontos"

-- | Funções obsoletas mantidas por compatibilidade
exibirRankingAcumulado :: Int -> IO ()
exibirRankingAcumulado = exibirRankingMelhores

exibirRanking :: Int -> IO ()
exibirRanking = exibirHistoricoPartidas

-- Corrigido para converter o tipo PontuacaoJogador para (String, Int)
lerRanking :: IO [(String, Int)]
lerRanking = do
    historico <- lerHistoricoPartidas
    return [(nome, pontos) | (nome, pontos, _, _) <- historico]

arquivoRanking :: FilePath
arquivoRanking = arquivoHistoricoPartidas