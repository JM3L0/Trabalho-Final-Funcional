-- | Funções utilitárias diversas para o Jogo da Forca.
-- Este módulo agrupa funcionalidades auxiliares como o banco de palavras,
-- seleção aleatória de palavras e limpeza da tela do console.
module Utilitarios (
    -- * Banco de Palavras
    selecionarPalavra,  -- ^ Seleciona uma palavra aleatória do arquivo.
    arquivoPalavras,    -- ^ Caminho do arquivo de palavras.
    -- * Funções de Terminal
    limparTela          -- ^ Limpa a tela do console.
) where

import System.IO (hFlush, stdout, readFile, writeFile)
-- Importa System.Random para uma melhor geração de números aleatórios.
import System.Random (randomRIO)
-- Importações adicionais para tratamento de erros e manipulação de strings
import System.IO.Error (catchIOError, isDoesNotExistError)
import Data.Char (toUpper, isSpace)
import Control.Exception (try)

-- | Caminho para o arquivo que contém as palavras do jogo.
-- Agora usa múltiplos caminhos possíveis para encontrar o arquivo.
arquivoPalavras :: FilePath
arquivoPalavras = "./data/palavras.txt"  -- Agora usa caminho relativo ao diretório de execução

-- | Lista de caminhos alternativos onde o arquivo pode estar.
caminhosPossiveis :: [FilePath]
caminhosPossiveis = [
    "./data/palavras.txt",        -- Relativo ao diretório atual
    "../data/palavras.txt",       -- Relativo ao diretório pai
    "palavras.txt",               -- No próprio diretório
    "./palavras.txt"              -- Alternativa explícita
  ]

-- | Cria o arquivo de palavras se ele não existir.
-- Esta função deve ser chamada antes da primeira leitura.
inicializarArquivoPalavras :: IO ()
inicializarArquivoPalavras = do
    -- Tenta encontrar o arquivo em um dos caminhos possíveis
    arquivoEncontrado <- encontrarArquivo caminhosPossiveis
    
    case arquivoEncontrado of
        -- Se encontrou o arquivo, não precisa fazer nada
        Just caminho -> putStrLn $ "Usando arquivo de palavras: " ++ caminho
        -- Se não encontrou, tenta criar nos caminhos possíveis
        Nothing -> do
            putStrLn "Arquivo de palavras não encontrado, tentando criar..."
            criarArquivoPalavras
            
-- | Tenta encontrar o arquivo em uma lista de caminhos possíveis.
encontrarArquivo :: [FilePath] -> IO (Maybe FilePath)
encontrarArquivo [] = return Nothing
encontrarArquivo (caminho:restos) = do
    existe <- catchIOError (readFile caminho >> return True) (\_ -> return False)
    if existe
        then return (Just caminho)
        else encontrarArquivo restos

-- | Tenta criar o arquivo de palavras em vários caminhos possíveis.
criarArquivoPalavras :: IO ()
criarArquivoPalavras = do
    -- Tenta criar o diretório data
    _ <- catchIOError (writeFile "data/palavras.txt" exemplosPalavras) (\_ -> return ())
    -- Tenta criar no diretório atual
    _ <- catchIOError (writeFile "palavras.txt" exemplosPalavras) (\_ -> return ())
    return ()
    where
        exemplosPalavras = "HASKELL\nPROGRAMACAO\nFUNCIONAL\nCOMPUTACAO\nALGORITMO"

-- | Lê o banco de palavras do arquivo, com tratamento de erros.
-- Se o arquivo não existir ou ocorrer outro erro, mostra um erro claro.
lerBancoPalavras :: IO [String]
lerBancoPalavras = do
    -- Tenta encontrar o arquivo em um dos caminhos possíveis
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

-- | Lê palavras de um arquivo específico
lerPalavrasDoArquivo :: FilePath -> IO [String]
lerPalavrasDoArquivo caminho = do
  conteudo <- readFile caminho
  -- Converte cada linha para maiúsculas, remove espaços extras e filtra linhas vazias
  let palavras = filter (not . null) (map (map toUpper . trim) (lines conteudo))
  -- Se o arquivo estiver vazio, usa palavras de emergência
  if null palavras
    then do
        putStrLn $ "AVISO: O arquivo " ++ caminho ++ " está vazio. Usando banco de palavras de emergência."
        return ["HASKELL", "COMPUTADOR", "PROGRAMA", "FUNCIONAL"]
    else return palavras

-- | Seleciona uma palavra aleatória do banco de palavras.
selecionarPalavra :: IO String
selecionarPalavra = do
  -- Inicializa e tenta ler o banco de palavras
  inicializarArquivoPalavras
  palavras <- lerBancoPalavras
  
  -- Verifica se há palavras disponíveis
  if null palavras
    then do
      putStrLn "AVISO: Nenhuma palavra disponível no banco. Usando palavra padrão 'HASKELL'."
      return "HASKELL"
    else do
      let tamanhoBanco = length palavras
      -- Gera um índice aleatório entre 0 e (tamanhoBanco - 1).
      indice <- randomRIO (0, tamanhoBanco - 1)
      -- Retorna a palavra no índice selecionado.
      return (palavras !! indice)

-- | Função auxiliar para remover espaços em branco no início e fim de uma string.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Limpa a tela do terminal e posiciona o cursor no início.
-- Demonstra uma ação de IO que interage com o terminal.
-- Usa sequências de escape ANSI padrão para compatibilidade com a maioria dos terminais.
limparTela :: IO ()
limparTela = do
  putStr "\ESC[2J"  -- Código ANSI para limpar a tela (Clear Screen).
  putStr "\ESC[H"   -- Código ANSI para mover o cursor para a posição Home (topo esquerdo).
  hFlush stdout    -- Garante que os comandos de limpeza sejam enviados imediatamente ao terminal.

