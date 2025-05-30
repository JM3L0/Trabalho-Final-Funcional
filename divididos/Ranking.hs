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
import Tipos (Jogo, estadoJogo, EstadoJogo(Ganhou))
import LogicaJogo (calcularPontuacao)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
-- Importa funções de IO e tratamento de erro
import System.IO (readFile, writeFile, IOMode(AppendMode), withFile, hPutStrLn, hGetContents, Handle)
import System.IO.Error (catchIOError, isDoesNotExistError)
import Text.Read (readMaybe)

-- | Representa a pontuação de um jogador como uma tupla (Nome, Pontos).
-- Usando `type` para criar um sinônimo, melhorando a legibilidade.
type PontuacaoJogador = (String, Int)

-- | Caminho relativo para o arquivo que armazena o histórico de partidas.
-- O arquivo é esperado dentro de um diretório 'data' no nível pai do 'src'.
arquivoHistoricoPartidas :: FilePath
arquivoHistoricoPartidas = "../data/historico_partidas.txt"

-- | Caminho relativo para o arquivo que armazena o ranking acumulado.
arquivoRankingAcumulado :: FilePath
arquivoRankingAcumulado = "../data/ranking_acumulado.txt"

-- | Lê as pontuações do arquivo de histórico de partidas.
-- Demonstra tratamento de erros de IO usando `catchIOError`.
-- Se o arquivo não existir ou ocorrer outro erro de leitura, retorna uma lista vazia
-- e tenta criar o arquivo (se não existir) para a próxima execução.
--
-- Retorna:
--   IO [PontuacaoJogador]: Uma ação de IO que resulta na lista de pontuações lidas.
lerHistoricoPartidas :: IO [PontuacaoJogador]
lerHistoricoPartidas = catchIOError lerHistoricoPartidasInterno tratarErroLeitura
  where
    tratarErroLeitura e =
      if isDoesNotExistError e
        then do
          putStrLn $ "Arquivo de histórico (" ++ arquivoHistoricoPartidas ++ ") não encontrado. Será criado um novo."
          -- Tenta criar o arquivo vazio. Ignora erro se não conseguir criar.
          catchIOError (writeFile arquivoHistoricoPartidas "") (\_ -> return ())
          return [] -- Retorna lista vazia pois não havia histórico.
        else do
          putStrLn $ "Erro ao ler o arquivo de histórico: " ++ show e
          return [] -- Retorna lista vazia em caso de outros erros.

-- | Função interna que realiza a leitura e parsing do arquivo.
lerHistoricoPartidasInterno :: IO [PontuacaoJogador]
lerHistoricoPartidasInterno = do
    -- Usa `readFile` em vez de `withFile` para evitar problemas com lazy IO
    conteudo <- readFile arquivoHistoricoPartidas
    let linhas = lines conteudo
    -- Usa uma função `mapMaybe` local para processar as linhas,
    -- descartando aquelas que não puderem ser parseadas corretamente.
    let pontuacoes = mapMaybe parseLinhaPontuacao linhas
    return pontuacoes

-- | Tenta converter uma linha do arquivo "Nome Pontos" em uma `PontuacaoJogador`.
-- Retorna `Nothing` se a linha estiver mal formatada.
-- Demonstra o uso de `words`, `case` e `readMaybe` para parsing seguro.
parseLinhaPontuacao :: String -> Maybe PontuacaoJogador
parseLinhaPontuacao linha = case words linha of
    -- Pega o primeiro nome e a string de pontos.
    (nome:pontosStr:_) -> case readMaybe pontosStr :: Maybe Int of
                            Just pontos -> Just (nome, pontos)
                            Nothing     -> Nothing -- Falha ao converter pontos para Int
    _                  -> Nothing -- Linha não tem pelo menos duas palavras

-- | Lê as pontuações do arquivo de ranking acumulado.
-- Similar a `lerRanking`, mas para o arquivo de ranking acumulado.
lerRankingAcumulado :: IO [PontuacaoJogador]
lerRankingAcumulado = catchIOError lerRankingAcumuladoInterno tratarErroLeitura
  where
    tratarErroLeitura e =
      if isDoesNotExistError e
        then do
          putStrLn $ "Arquivo de ranking acumulado (" ++ arquivoRankingAcumulado ++ ") não encontrado. Será criado um novo."
          catchIOError (writeFile arquivoRankingAcumulado "") (\_ -> return ())
          return [] 
        else do
          putStrLn $ "Erro ao ler o arquivo de ranking acumulado: " ++ show e
          return [] 

-- | Função interna que realiza a leitura e parsing do arquivo acumulado.
lerRankingAcumuladoInterno :: IO [PontuacaoJogador]
lerRankingAcumuladoInterno = do
    conteudo <- readFile arquivoRankingAcumulado
    let linhas = lines conteudo
    let pontuacoes = mapMaybe parseLinhaPontuacao linhas
    return pontuacoes

-- | Salva a pontuação de um jogador em ambos os arquivos de ranking.
-- No histórico de partidas, salva cada partida separadamente.
-- No ranking acumulado, soma com a pontuação existente do jogador (se houver).
salvarPontuacao :: String -> Jogo -> IO ()
salvarPontuacao nomeJogador jogo
  -- Guarda: Só salva se o jogador ganhou.
  | estadoJogo jogo == Ganhou = do
      let pontuacao = calcularPontuacao jogo
      -- Não salva pontuações de 0 ou menos (embora não deva ocorrer se ganhou)
      if pontuacao > 0
          then do
              -- Salva no histórico de partidas individuais
              let linha = nomeJogador ++ " " ++ show pontuacao
              -- Tenta adicionar a linha ao arquivo.
              catchIOError (withFile arquivoHistoricoPartidas AppendMode (\h -> hPutStrLn h linha))
                           tratarErroEscrita
              
              -- Atualiza o ranking acumulado (soma total por jogador)
              atualizarRankingAcumulado nomeJogador pontuacao
          else return () -- Não salva pontuação zero ou negativa
  -- Caso contrário (não ganhou), não faz nada.
  | otherwise = return ()
  where
    tratarErroEscrita e = putStrLn $ "Erro ao salvar pontuação no arquivo " ++ arquivoHistoricoPartidas ++ ": " ++ show e

-- | Atualiza a pontuação de um jogador na lista de pontuações.
-- Se o jogador já existe, soma a nova pontuação. Caso contrário, adiciona-o à lista.
atualizarPontuacaoJogador :: String -> Int -> [PontuacaoJogador] -> [PontuacaoJogador]
atualizarPontuacaoJogador nome novaPontuacao [] = [(nome, novaPontuacao)]
atualizarPontuacaoJogador nome novaPontuacao ((jogador, pontos):resto)
    | jogador == nome = (jogador, pontos + novaPontuacao) : resto
    | otherwise       = (jogador, pontos) : atualizarPontuacaoJogador nome novaPontuacao resto

-- | Atualiza o ranking acumulado com a nova pontuação do jogador.
-- Lê o ranking existente, atualiza a pontuação do jogador e reescreve o arquivo.
atualizarRankingAcumulado :: String -> Int -> IO ()
atualizarRankingAcumulado nomeJogador novaPontuacao = do
    rankingAtual <- lerRankingAcumulado
    
    -- Adicionar log para diagnóstico
    putStrLn $ "Ranking atual: " ++ show rankingAtual
    putStrLn $ "Adicionando " ++ show novaPontuacao ++ " pontos para " ++ nomeJogador
    
    let rankingAtualizado = atualizarPontuacaoJogador nomeJogador novaPontuacao rankingAtual
    
    putStrLn $ "Ranking atualizado: " ++ show rankingAtualizado
    
    -- Reescreve o arquivo com o ranking atualizado
    let conteudo = unlines (map formatarPontuacao rankingAtualizado)
    catchIOError (writeFile arquivoRankingAcumulado conteudo)
                 (\e -> putStrLn $ "Erro ao atualizar o ranking acumulado: " ++ show e)
  where
    formatarPontuacao (nome, pontos) = nome ++ " " ++ show pontos

-- | Exibe o ranking dos melhores jogadores no console baseado na pontuação acumulada.
-- Lê o ranking acumulado, ordena por pontuação (decrescente) e exibe os `topN` primeiros.
--
-- Argumentos:
--   topN: O número de melhores jogadores a serem exibidos.
exibirRankingMelhores :: Int -> IO ()
exibirRankingMelhores topN = do
    putStrLn "\n--- Ranking dos Melhores Jogadores ---"
    rankingCompleto <- lerRankingAcumulado
    if null rankingCompleto
        then putStrLn "Nenhuma pontuação registrada ainda."
        else do
            -- Ordena a lista de pontuações em ordem decrescente de pontos
            let rankingOrdenado = sortBy (\(_, p1) (_, p2) -> compare p2 p1) rankingCompleto
            -- Pega os `topN` primeiros da lista ordenada
            let rankingTop = take topN rankingOrdenado
            -- Usa `zip` para adicionar a posição (1, 2, 3...) e `mapM_` para imprimir cada entrada
            mapM_ imprimirEntradaRanking (zip [1..] rankingTop)
    putStrLn "------------------------------------\n"

-- | Exibe o histórico das partidas mais recentes.
-- Lê o histórico de partidas, inverte a ordem para mostrar as mais recentes primeiro.
--
-- Argumentos:
--   numPartidas: O número de partidas recentes a serem exibidas.
exibirHistoricoPartidas :: Int -> IO ()
exibirHistoricoPartidas numPartidas = do
    putStrLn "\n--- Histórico de Partidas Recentes ---"
    historicoCompleto <- lerHistoricoPartidas
    if null historicoCompleto
        then putStrLn "Nenhuma partida registrada ainda."
        else do
            -- Inverte a lista para mostrar as partidas mais recentes primeiro
            -- (assumindo que novas partidas são adicionadas ao final do arquivo)
            let historicoReverso = reverse historicoCompleto
            -- Pega as `numPartidas` mais recentes
            let historicoRecente = take numPartidas historicoReverso
            -- Imprime cada partida
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

