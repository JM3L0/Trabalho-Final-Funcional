-- Utilitarios.hs
-- | Funções utilitárias diversas para o Jogo da Forca.
module Utilitarios (
    selecionarPalavra,     -- Seleciona uma palavra aleatória do banco de palavras
    arquivoPalavras,       -- Caminho padrão para o arquivo de palavras
    limparTela,            -- Limpa o terminal
    trim                   -- Remove espaços em branco no início e fim de uma string
) where

import System.IO (hFlush, stdout, readFile, writeFile)
import System.Random (randomRIO)
import System.IO.Error (catchIOError)
import Data.Char (toUpper, isSpace)
import Control.Monad (unless)

-- | Caminho padrão para o arquivo de palavras
arquivoPalavras :: FilePath
arquivoPalavras = "data/palavras.txt"

-- | Lista de possíveis caminhos onde o arquivo de palavras pode estar
caminhosPossiveis :: [FilePath]
caminhosPossiveis = [
    "data/palavras.txt",    -- Caminho relativo ao diretório atual
    "palavras.txt",         -- No diretório atual
    "../data/palavras.txt", -- Diretório acima
    "./palavras.txt"        -- Diretório atual com ./ explícito
  ]

-- | Inicializa o arquivo de palavras, verificando se existe ou criando-o se necessário
inicializarArquivoPalavras :: IO ()
inicializarArquivoPalavras = do
    arquivoEncontrado <- encontrarArquivo caminhosPossiveis
    case arquivoEncontrado of
        Just caminho -> putStrLn $ "Usando arquivo de palavras: " ++ caminho
        Nothing -> do
            putStrLn "Arquivo de palavras não encontrado, tentando criar..."
            criarArquivoPalavras

-- | Procura o arquivo de palavras nos caminhos possíveis
encontrarArquivo :: [FilePath] -> IO (Maybe FilePath)
encontrarArquivo [] = return Nothing  -- Nenhum arquivo encontrado
encontrarArquivo (caminho:restos) = do
    existe <- catchIOError (readFile caminho >> return True) (\_ -> return False)
    if existe
        then return (Just caminho)  -- Arquivo encontrado
        else encontrarArquivo restos  -- Tenta o próximo caminho

-- | Cria um arquivo de palavras com exemplos básicos
criarArquivoPalavras :: IO ()
criarArquivoPalavras = do
    tentarCriarArquivo caminhosPossiveis
    where
        -- Tenta criar o arquivo em cada caminho possível
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
        -- Palavras exemplo para inicializar o arquivo
        exemplosPalavras = "HASKELL\nPROGRAMACAO\nFUNCIONAL\nCOMPUTACAO\nALGORITMO"

-- | Lê todas as palavras do banco de palavras
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
    -- Tratamento de erro na leitura do arquivo
    tratarErroLeitura e = do
      putStrLn $ "ERRO ao ler arquivo: " ++ show e
      putStrLn "Usando banco de palavras de emergência."
      return palavrasEmergencia
    -- Palavras de emergência caso o arquivo não possa ser lido
    palavrasEmergencia = ["HASKELL", "COMPUTADOR", "PROGRAMA", "FUNCIONAL"]

-- | Lê palavras de um arquivo específico
lerPalavrasDoArquivo :: FilePath -> IO [String]
lerPalavrasDoArquivo caminho = do
  conteudo <- readFile caminho
  let palavras = filter (not . null) (map (map toUpper . trim) (lines conteudo))
  if null palavras
    then do
        putStrLn $ "AVISO: O arquivo " ++ caminho ++ " está vazio. Usando banco de palavras de emergência."
        return ["HASKELL", "COMPUTADOR", "PROGRAMA", "FUNCIONAL"]
    else return palavras

-- | Seleciona uma palavra aleatória do banco de palavras
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

-- | Remove espaços em branco no início e no fim de uma string
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Limpa o terminal usando códigos de escape ANSI
limparTela :: IO ()
limparTela = do
  putStr "\ESC[2J"  -- Limpa toda a tela
  putStr "\ESC[H"   -- Move o cursor para o início (0,0)
  hFlush stdout     -- Garante que os comandos anteriores sejam executados imediatamente