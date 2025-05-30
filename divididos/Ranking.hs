-- | Gerencia o sistema de pontuação e ranking do Jogo da Forca.
-- Este módulo lida com a persistência de dados (leitura e escrita em arquivo),
-- tratamento de erros de IO e manipulação de listas para ordenação.
module Ranking (
  -- * Tipos e Constantes
  PontuacaoJogador,     -- ^ Alias de tipo para representar (Nome, Pontos).
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
import Control.Concurrent (threadDelay)
import System.Directory (getCurrentDirectory)

-- | Obtém o caminho absoluto para um arquivo no diretório atual
obterCaminhoAbsoluto :: FilePath -> IO FilePath
obterCaminhoAbsoluto arquivo = do
    dir <- getCurrentDirectory
    return (dir ++ "\\" ++ arquivo)

-- | Caminho para o arquivo de histórico - usando nome simples para resolver via função
arquivoHistoricoPartidas :: FilePath
arquivoHistoricoPartidas = "historico.txt"

-- | Caminho para o arquivo de ranking acumulado - usando nome simples para resolver via função
arquivoRankingAcumulado :: FilePath
arquivoRankingAcumulado = "ranking.txt"

-- | Função simplificada para garantir que um arquivo existe
garantirArquivoExiste :: FilePath -> IO ()
garantirArquivoExiste arquivo = do
    caminho <- obterCaminhoAbsoluto arquivo
    existe <- catchIOError (readFile caminho >> return True) (\_ -> return False)
    if not existe
        then do
            putStrLn $ "Criando arquivo: " ++ caminho
            -- Tenta várias vezes com delay entre tentativas
            tentarEscrever caminho "" 3
        else return ()
  where
    tentarEscrever _ _ 0 = putStrLn "Falha ao criar arquivo após várias tentativas"
    tentarEscrever caminho conteudo tentativas = do
        sucesso <- catch (writeFile caminho conteudo >> return True)
                         (\(_ :: SomeException) -> return False)
        if not sucesso
            then do
                threadDelay 500000  -- 500ms
                tentarEscrever caminho conteudo (tentativas - 1)
            else return ()

-- | Escreve em um arquivo com tratamento de erros aprimorado
escreverArquivoRobusto :: FilePath -> String -> IO Bool
escreverArquivoRobusto arquivo conteudo = do
    caminho <- obterCaminhoAbsoluto arquivo
    tentarEscrever caminho conteudo 5  -- Tenta até 5 vezes
  where
    tentarEscrever _ _ 0 = return False
    tentarEscrever caminho conteudo tentativas = do
        sucesso <- catch (do
                        writeFile caminho conteudo
                        return True)
                      (\(e :: SomeException) -> do
                        putStrLn $ "Tentativa " ++ show (6 - tentativas) ++ " falhou: " ++ show e
                        threadDelay (500000 * (6 - tentativas))  -- Aumenta o delay progressivamente
                        return False)
        if sucesso 
            then return True
            else tentarEscrever caminho conteudo (tentativas - 1)

-- | Tenta converter uma linha do arquivo "Nome Pontos" em uma `PontuacaoJogador`.
-- Retorna `Nothing` se a linha estiver mal formatada.
parseLinhaPontuacao :: String -> Maybe PontuacaoJogador
parseLinhaPontuacao linha = case words linha of
    (nome:pontosStr:_) -> case readMaybe pontosStr :: Maybe Int of
                            Just pontos -> Just (nome, pontos)
                            Nothing     -> Nothing
    _ -> Nothing

-- | Atualiza a pontuação de um jogador na lista de pontuações.
-- Se o jogador já existe, soma a nova pontuação. Caso contrário, adiciona-o à lista.
atualizarPontuacaoJogador :: String -> Int -> [PontuacaoJogador] -> [PontuacaoJogador]
atualizarPontuacaoJogador nome novaPontuacao [] = [(nome, novaPontuacao)]
atualizarPontuacaoJogador nome novaPontuacao ((jogador, pontos):resto)
    | jogador == nome = (jogador, pontos + novaPontuacao) : resto
    | otherwise       = (jogador, pontos) : atualizarPontuacaoJogador nome novaPontuacao resto

-- | Lê as pontuações do arquivo de histórico de partidas - Versão ultra simplificada
lerHistoricoPartidas :: IO [PontuacaoJogador]
lerHistoricoPartidas = do
    -- Garante que o arquivo existe
    garantirArquivoExiste arquivoHistoricoPartidas
    
    -- Lê o conteúdo com caminho absoluto
    caminho <- obterCaminhoAbsoluto arquivoHistoricoPartidas
    
    -- Lê conteúdo e processa
    conteudo <- catch (readFile caminho) 
                      (\(_ :: SomeException) -> return "")
    
    putStrLn $ "Conteúdo do histórico: " ++ conteudo
    
    -- Converte para lista de pontuações
    let linhas = lines conteudo
    return (mapMaybe parseLinhaPontuacao linhas)

-- | Lê as pontuações do arquivo de ranking acumulado - Versão ultra simplificada
lerRankingAcumulado :: IO [PontuacaoJogador]
lerRankingAcumulado = do
    -- Garante que o arquivo existe
    garantirArquivoExiste arquivoRankingAcumulado
    
    -- Lê o conteúdo com caminho absoluto
    caminho <- obterCaminhoAbsoluto arquivoRankingAcumulado
    
    -- Lê conteúdo e processa
    conteudo <- catch (readFile caminho) 
                      (\(_ :: SomeException) -> return "")
    
    -- Converte para lista de pontuações
    let linhas = lines conteudo
    let resultado = mapMaybe parseLinhaPontuacao linhas
    
    putStrLn $ "Ranking lido: " ++ show resultado
    return resultado

-- | Registra um resultado de partida (ganhou ou perdeu)
-- Esta função permite registrar o resultado de qualquer partida, mesmo que o jogador perca
registrarResultadoPartida :: String -> Jogo -> Bool -> IO ()
registrarResultadoPartida nomeJogador jogo ganhou = do
    garantirArquivoExiste arquivoHistoricoPartidas
    garantirArquivoExiste arquivoRankingAcumulado
    
    -- Calcula pontuação (será 0 se perdeu)
    let pontuacao = if ganhou then calcularPontuacao jogo else 0
    
    -- Registro da partida no histórico com informações extras para diagnóstico
    let resultado = if ganhou then "venceu" else "perdeu"
    let linha = nomeJogador ++ " " ++ show pontuacao ++ " (" ++ resultado ++ " - palavra: " ++ palavraSecreta jogo ++ ")\n"
    
    putStrLn $ "Salvando no histórico: " ++ linha
    
    -- Método alternativo: usar writeFile após ler o conteúdo
    conteudoAtual <- lerArquivoSeguro arquivoHistoricoPartidas
    let novoConteudo = conteudoAtual ++ linha
    
    sucesso <- catch (writeFile arquivoHistoricoPartidas novoConteudo >> return True) 
                      (\(_ :: SomeException) -> putStrLn "Falha ao salvar histórico" >> return False)
    
    if not sucesso
        then putStrLn "Não foi possível salvar no histórico de partidas."
        else putStrLn "Histórico atualizado com sucesso."
    
    -- Atualiza o ranking acumulado apenas se ganhou e pontuação > 0
    if ganhou && pontuacao > 0
        then do
            rankingAtualizado <- atualizarRankingSimplesAcumulado nomeJogador pontuacao
            putStrLn $ "Ranking atualizado: " ++ show rankingAtualizado
        else return ()

-- | Salva a pontuação de um jogador - Versão altamente simplificada
salvarPontuacao :: String -> Jogo -> IO ()
salvarPontuacao nomeJogador jogo
  | estadoJogo jogo == Ganhou = do
      let pontuacao = calcularPontuacao jogo
      
      -- Tenta salvar no histórico (método direto)
      caminhoHistorico <- obterCaminhoAbsoluto arquivoHistoricoPartidas
      let linhaHistorico = nomeJogador ++ " " ++ show pontuacao ++ "\n"
      
      -- Garante que o arquivo existe
      garantirArquivoExiste arquivoHistoricoPartidas
      
      -- Lê conteúdo atual do histórico
      conteudoHistorico <- readFile caminhoHistorico
      
      -- Adiciona nova linha e escreve tudo de volta
      putStrLn $ "Salvando pontuação no histórico: " ++ linhaHistorico
      sucesso <- escreverArquivoRobusto arquivoHistoricoPartidas (conteudoHistorico ++ linhaHistorico)
      
      if not sucesso
          then putStrLn "FALHA AO SALVAR HISTÓRICO"
          else putStrLn "Histórico salvo com sucesso"
      
      -- Tenta atualizar o ranking acumulado (método direto)
      putStrLn "Atualizando ranking acumulado..."
      garantirArquivoExiste arquivoRankingAcumulado
      caminhoRanking <- obterCaminhoAbsoluto arquivoRankingAcumulado
      
      -- Lê ranking atual
      conteudoRanking <- readFile caminhoRanking
      
      -- Processa o ranking
      let linhas = lines conteudoRanking
      let ranking = mapMaybe parseLinhaPontuacao linhas
      
      -- Atualiza a pontuação do jogador
      let encontrado = filter (\(nome, _) -> nome == nomeJogador) ranking
      let outrosJogadores = filter (\(nome, _) -> nome /= nomeJogador) ranking
      
      let novaPontuacaoTotal = case encontrado of
                                [] -> pontuacao
                                [(_, pontosAntigos)] -> pontosAntigos + pontuacao
      
      let rankingAtualizado = (nomeJogador, novaPontuacaoTotal) : outrosJogadores
      let conteudoNovoRanking = unlines [nome ++ " " ++ show pontos | (nome, pontos) <- rankingAtualizado]
      
      -- Salva o ranking atualizado
      putStrLn $ "Salvando novo ranking: " ++ conteudoNovoRanking
      sucessoRanking <- escreverArquivoRobusto arquivoRankingAcumulado conteudoNovoRanking
      
      if not sucessoRanking
          then putStrLn "FALHA AO SALVAR RANKING ACUMULADO"
          else putStrLn "Ranking acumulado salvo com sucesso"
      
  | otherwise = return ()

-- | Versão corrigida da atualização do ranking acumulado
atualizarRankingSimplesAcumulado :: String -> Int -> IO [PontuacaoJogador]
atualizarRankingSimplesAcumulado nomeJogador novaPontuacao = do
    -- Lê o ranking atual
    ranking <- lerRankingAcumulado
    putStrLn $ "Ranking atual lido: " ++ show ranking
    
    -- Processamento simplificado do ranking
    let atualizarPontuacao [] = [(nomeJogador, novaPontuacao)]
        atualizarPontuacao ((nome, pontos):resto)
            | nome == nomeJogador = (nome, pontos + novaPontuacao) : resto
            | otherwise = (nome, pontos) : atualizarPontuacao resto
    
    let rankingAtualizado = atualizarPontuacao ranking
    
    -- Formatação e escrita simplificadas
    let conteudo = unlines [nome ++ " " ++ show pontos | (nome, pontos) <- rankingAtualizado]
    
    putStrLn $ "Tentando salvar ranking: " ++ conteudo
    
    -- Escrita direta com tratamento de erro mais detalhado
    sucesso <- catch (writeFile arquivoRankingAcumulado conteudo >> return True)
                   (\(e :: SomeException) -> do
                        putStrLn $ "Erro ao salvar ranking: " ++ show e
                        return False)
    
    if sucesso
        then putStrLn "Ranking salvo com sucesso."
        else putStrLn "Não foi possível salvar o ranking acumulado."
    
    return rankingAtualizado

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
-- Função auxiliar para `exibirHistoricoPartidas`.
imprimirHistoricoPartida :: (Int, PontuacaoJogador) -> IO ()
imprimirHistoricoPartida (numero, (nome, pontos)) = do
    putStrLn $ show numero ++ ". " ++ nome ++ " marcou " ++ show pontos ++ " pontos"

-- | Imprime uma entrada formatada do ranking (posição, nome, pontos).
-- Função auxiliar para `exibirRankingMelhores`.
imprimirEntradaRanking :: (Int, PontuacaoJogador) -> IO ()
imprimirEntradaRanking (posicao, (nome, pontos)) = do
    putStrLn $ show posicao ++ ". " ++ nome ++ " - " ++ show pontos ++ " pontos"

-- As funções a seguir são mantidas por compatibilidade, mas delegam para as novas funções
-- | @Obsoleta@ Use `exibirRankingMelhores` no lugar.
exibirRankingAcumulado :: Int -> IO ()
exibirRankingAcumulado = exibirRankingMelhores

-- | @Obsoleta@ Use `exibirHistoricoPartidas` no lugar.
exibirRanking :: Int -> IO ()
exibirRanking = exibirHistoricoPartidas

-- | @Obsoleta@ Use `lerHistoricoPartidas` no lugar.
lerRanking :: IO [PontuacaoJogador]
lerRanking = lerHistoricoPartidas

-- | @Obsoleta@ Use `arquivoHistoricoPartidas` no lugar.
arquivoRanking :: FilePath
arquivoRanking = arquivoHistoricoPartidas

