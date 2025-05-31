-- Utilitarios.hs
-- | Funções utilitárias diversas para o Jogo da Forca.
module Utilitarios (
    selecionarPalavra,
    arquivoPalavras,
    limparTela,
    trim
) where

import System.IO (hFlush, stdout, readFile, writeFile)
import System.Random (randomRIO)
import System.IO.Error (catchIOError)
import Data.Char (toUpper, isSpace)
import Control.Monad (unless)

arquivoPalavras :: FilePath
arquivoPalavras = "data/palavras.txt"

caminhosPossiveis :: [FilePath]
caminhosPossiveis = [
    "data/palavras.txt",
    "palavras.txt",
    "../data/palavras.txt",
    "./palavras.txt"
  ]

inicializarArquivoPalavras :: IO ()
inicializarArquivoPalavras = do
    arquivoEncontrado <- encontrarArquivo caminhosPossiveis
    case arquivoEncontrado of
        Just caminho -> putStrLn $ "Usando arquivo de palavras: " ++ caminho
        Nothing -> do
            putStrLn "Arquivo de palavras não encontrado, tentando criar..."
            criarArquivoPalavras

encontrarArquivo :: [FilePath] -> IO (Maybe FilePath)
encontrarArquivo [] = return Nothing
encontrarArquivo (caminho:restos) = do
    existe <- catchIOError (readFile caminho >> return True) (\_ -> return False)
    if existe
        then return (Just caminho)
        else encontrarArquivo restos

criarArquivoPalavras :: IO ()
criarArquivoPalavras = do
    tentarCriarArquivo caminhosPossiveis
    where
        tentarCriarArquivo [] = putStrLn "Não foi possível criar o arquivo de palavras em nenhum local."
        tentarCriarArquivo (caminho:restos) = do
            resultado <- catchIOError
                (do
                    writeFile caminho exemplosPalavras
                    putStrLn $ "Arquivo de palavras criado com sucesso em: " ++ caminho
                    return True)
                (\e -> do
                    putStrLn $ "Erro ao criar " ++ caminho ++ ": " ++ show e
                    return False)
            unless resultado $ tentarCriarArquivo restos
        exemplosPalavras = "HASKELL\nPROGRAMACAO\nFUNCIONAL\nCOMPUTACAO\nALGORITMO"

lerBancoPalavras :: IO [String]
lerBancoPalavras = do
    arquivoEncontrado <- encontrarArquivo caminhosPossiveis
    case arquivoEncontrado of
        Just caminho -> catchIOError (lerPalavrasDoArquivo caminho) tratarErroLeitura
        Nothing -> do
            putStrLn "ERRO: Não foi possível encontrar o arquivo de palavras em nenhum dos seguintes locais:"
            mapM_ (\p -> putStrLn $ "  - " ++ p) caminhosPossiveis
            putStrLn "Usando banco de palavras de emergência."
            return palavrasEmergencia
  where
    tratarErroLeitura e = do
      putStrLn $ "ERRO ao ler arquivo: " ++ show e
      putStrLn "Usando banco de palavras de emergência."
      return palavrasEmergencia
    palavrasEmergencia = ["HASKELL", "COMPUTADOR", "PROGRAMA", "FUNCIONAL"]

lerPalavrasDoArquivo :: FilePath -> IO [String]
lerPalavrasDoArquivo caminho = do
  conteudo <- readFile caminho
  let palavras = filter (not . null) (map (map toUpper . trim) (lines conteudo))
  if null palavras
    then do
        putStrLn $ "AVISO: O arquivo " ++ caminho ++ " está vazio. Usando banco de palavras de emergência."
        return ["HASKELL", "COMPUTADOR", "PROGRAMA", "FUNCIONAL"]
    else return palavras

selecionarPalavra :: IO String
selecionarPalavra = do
  inicializarArquivoPalavras
  palavras <- lerBancoPalavras
  if null palavras
    then do
      putStrLn "AVISO: Nenhuma palavra disponível no banco. Usando palavra padrão 'HASKELL'."
      return "HASKELL"
    else do
      let tamanhoBanco = length palavras
      indice <- randomRIO (0, tamanhoBanco - 1)
      return (palavras !! indice)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

limparTela :: IO ()
limparTela = do
  putStr "\ESC[2J"
  putStr "\ESC[H"
  hFlush stdout