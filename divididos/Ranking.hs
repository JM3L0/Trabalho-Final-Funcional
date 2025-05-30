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
import System.IO (readFile, writeFile, appendFile, hClose, openFile, IOMode(AppendMode, WriteMode, ReadMode))
import System.IO.Error (catchIOError, isDoesNotExistError)
import Text.Read (readMaybe)
import Control.Exception (try, catch, SomeException)

-- | Representa a pontuação de um jogador como uma tupla (Nome, Pontos).
type PontuacaoJogador = (String, Int)

-- | Caminho para o arquivo de histórico - simplificado para o diretório atual
arquivoHistoricoPartidas :: FilePath
arquivoHistoricoPartidas = "historico_partidas.txt"

-- | Caminho para o arquivo de ranking acumulado - simplificado para o diretório atual
arquivoRankingAcumulado :: FilePath
arquivoRankingAcumulado = "ranking_acumulado.txt"

-- | Função simplificada para garantir que um arquivo existe
garantirArquivoExiste :: FilePath -> IO ()
garantirArquivoExiste arquivo = do
    existe <- catchIOError (readFile arquivo >> return True) (\_ -> return False)
    if not existe
        then catch (writeFile arquivo "") (\(_ :: SomeException) -> putStrLn $ "Aviso: Não foi possível criar " ++ arquivo)
        else return ()

-- | Lê o conteúdo de um arquivo com tratamento de erros
lerArquivoSeguro :: FilePath -> IO String
lerArquivoSeguro arquivo = catch (readFile arquivo) handler
  where
    handler :: SomeException -> IO String
    handler _ = return ""

-- | Escreve em um arquivo com tratamento de erros
escreverArquivoSeguro :: FilePath -> String -> IO Bool
escreverArquivoSeguro arquivo conteudo = catch (writeFile arquivo conteudo >> return True) handler
  where
    handler :: SomeException -> IO Bool
    handler _ = return False

-- | Acrescenta ao final de um arquivo com tratamento de erros
acrescentarArquivoSeguro :: FilePath -> String -> IO Bool
acrescentarArquivoSeguro arquivo conteudo = catch (appendFile arquivo conteudo >> return True) handler
  where
    handler :: SomeException -> IO Bool
    handler _ = return False

-- | Lê as pontuações do arquivo de histórico de partidas - Versão simplificada
lerHistoricoPartidas :: IO [PontuacaoJogador]
lerHistoricoPartidas = do
    garantirArquivoExiste arquivoHistoricoPartidas
    conteudo <- lerArquivoSeguro arquivoHistoricoPartidas
    let linhas = lines conteudo
    return (mapMaybe parseLinhaPontuacao linhas)

-- | Lê as pontuações do arquivo de ranking acumulado - Versão simplificada
lerRankingAcumulado :: IO [PontuacaoJogador]
lerRankingAcumulado = do
    garantirArquivoExiste arquivoRankingAcumulado
    conteudo <- lerArquivoSeguro arquivoRankingAcumulado
    let linhas = lines conteudo
    return (mapMaybe parseLinhaPontuacao linhas)

-- | Salva a pontuação de um jogador - Versão simplificada e robusta
salvarPontuacao :: String -> Jogo -> IO ()
salvarPontuacao nomeJogador jogo
  | estadoJogo jogo == Ganhou = do
      let pontuacao = calcularPontuacao jogo
      if pontuacao > 0
          then do
              -- Garante que os arquivos existem
              garantirArquivoExiste arquivoHistoricoPartidas
              garantirArquivoExiste arquivoRankingAcumulado
              
              -- Salva no histórico de partidas
              let linha = nomeJogador ++ " " ++ show pontuacao ++ "\n"
              sucesso <- acrescentarArquivoSeguro arquivoHistoricoPartidas linha
              if not sucesso
                  then putStrLn "Não foi possível salvar no histórico de partidas."
                  else return ()
              
              -- Atualiza o ranking acumulado
              atualizarRankingSimplesAcumulado nomeJogador pontuacao
          else return ()
  | otherwise = return ()

-- | Versão simplificada da atualização do ranking acumulado
atualizarRankingSimplesAcumulado :: String -> Int -> IO ()
atualizarRankingSimplesAcumulado nomeJogador novaPontuacao = do
    ranking <- lerRankingAcumulado
    let rankingAtualizado = atualizarPontuacaoJogador nomeJogador novaPontuacao ranking
    let conteudo = unlines [nome ++ " " ++ show pontos | (nome, pontos) <- rankingAtualizado]
    sucesso <- escreverArquivoSeguro arquivoRankingAcumulado conteudo
    if not sucesso
        then putStrLn "Não foi possível atualizar o ranking acumulado."
        else return ()

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

