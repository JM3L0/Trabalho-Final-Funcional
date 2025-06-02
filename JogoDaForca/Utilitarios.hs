-- Utilitarios.hs
-- | Funções utilitárias diversas para o Jogo da Forca.
-- Este módulo contém funções de utilidade para manipulação de arquivos, 
-- seleção de palavras aleatórias e outras operações auxiliares.
module Utilitarios (
    selecionarPalavra,     -- Seleciona uma palavra aleatória do banco de palavras
    arquivoPalavras,       -- Caminho padrão para o arquivo de palavras
    limparTela,            -- Limpa o terminal
    trim                   -- Remove espaços em branco no início e fim de uma string
) where

import System.IO (hFlush, stdout, readFile, writeFile)  -- Operações de I/O
import System.Random (randomRIO)                        -- Geração de números aleatórios
import System.IO.Error (catchIOError)                   -- Tratamento de erros de I/O
import Data.Char (toUpper, isSpace)                     -- Manipulação de caracteres
import Control.Monad (unless)                           -- Operações condicionais em monads

-- | Caminho padrão para o arquivo de palavras.
-- Este é o local primário onde o programa tentará encontrar ou criar o arquivo de palavras.
arquivoPalavras :: FilePath
arquivoPalavras = "data/palavras.txt"

-- | Lista de possíveis caminhos onde o arquivo de palavras pode estar.
-- O programa tentará encontrar o arquivo em cada um destes caminhos, na ordem listada.
-- Esta abordagem aumenta a robustez do programa em diferentes ambientes de execução.
caminhosPossiveis :: [FilePath]
caminhosPossiveis = [
    "data/palavras.txt",    -- Caminho relativo ao diretório atual (pasta data)
    "palavras.txt",         -- No diretório atual (raiz)
    "../data/palavras.txt", -- Um nível acima, na pasta data
    "./palavras.txt"        -- Diretório atual com notação explícita
  ]

-- | Inicializa o arquivo de palavras, verificando se existe ou criando-o se necessário.
-- Esta função é chamada no início do jogo para garantir que haja um banco de palavras disponível.
inicializarArquivoPalavras :: IO ()
inicializarArquivoPalavras = do
    -- Tenta encontrar o arquivo em um dos caminhos possíveis
    arquivoEncontrado <- encontrarArquivo caminhosPossiveis
    case arquivoEncontrado of
        -- Se encontrou o arquivo, exibe uma mensagem informativa
        Just caminho -> putStrLn $ "Usando arquivo de palavras: " ++ caminho
        -- Se não encontrou, tenta criar um novo arquivo com palavras padrão
        Nothing -> do
            putStrLn "Arquivo de palavras não encontrado, tentando criar..."
            criarArquivoPalavras

-- | Procura o arquivo de palavras nos caminhos possíveis.
-- Retorna Just caminho se encontrar, ou Nothing se não encontrar em nenhum caminho.
-- Esta função tenta cada caminho em sequência até encontrar um arquivo válido.
encontrarArquivo :: [FilePath] -> IO (Maybe FilePath)
encontrarArquivo [] = return Nothing  -- Caso base: nenhum caminho restante, retorna Nothing
encontrarArquivo (caminho:restos) = do
    -- Tenta ler o arquivo para verificar se existe e é acessível
    existe <- catchIOError (readFile caminho >> return True) (\_ -> return False)
    if existe
        then return (Just caminho)  -- Arquivo encontrado, retorna o caminho
        else encontrarArquivo restos  -- Tenta o próximo caminho na lista

-- | Cria um arquivo de palavras com exemplos básicos.
-- Esta função é chamada quando nenhum arquivo de palavras é encontrado.
-- Tenta criar o arquivo em vários caminhos possíveis até conseguir.
criarArquivoPalavras :: IO ()
criarArquivoPalavras = do
    tentarCriarArquivo caminhosPossiveis
    where
        -- Função recursiva que tenta criar o arquivo em cada caminho possível
        tentarCriarArquivo [] = putStrLn "Não foi possível criar o arquivo de palavras em nenhum local."
        tentarCriarArquivo (caminho:restos) = do
            -- Tenta criar o arquivo no caminho atual, capturando possíveis erros
            resultado <- catchIOError
                (do
                    writeFile caminho exemplosPalavras  -- Escreve as palavras exemplo no arquivo
                    putStrLn $ "Arquivo de palavras criado com sucesso em: " ++ caminho
                    return True)
                (\e -> do
                    putStrLn $ "Erro ao criar " ++ caminho ++ ": " ++ show e
                    return False)
            -- Se não conseguiu criar neste caminho, tenta o próximo
            unless resultado $ tentarCriarArquivo restos
        -- Lista de palavras exemplo para inicializar o arquivo
        -- Estas palavras serão usadas caso nenhum arquivo existente seja encontrado
        exemplosPalavras = "HASKELL\nPROGRAMACAO\nFUNCIONAL\nCOMPUTACAO\nALGORITMO"

-- | Lê todas as palavras do banco de palavras.
-- Tenta encontrar e ler o arquivo de palavras. Se falhar, usa palavras de emergência.
-- As palavras são retornadas como uma lista de strings, todas em maiúsculas.
lerBancoPalavras :: IO [String]
lerBancoPalavras = do
    -- Tenta encontrar o arquivo em um dos caminhos possíveis
    arquivoEncontrado <- encontrarArquivo caminhosPossiveis
    case arquivoEncontrado of
        -- Se encontrou, tenta ler as palavras do arquivo, com tratamento de erros
        Just caminho -> catchIOError (lerPalavrasDoArquivo caminho) tratarErroLeitura
        -- Se não encontrou, exibe mensagem de erro e usa palavras de emergência
        Nothing -> do
            putStrLn "ERRO: Não foi possível encontrar o arquivo de palavras em nenhum dos seguintes locais:"
            mapM_ (\p -> putStrLn $ "  - " ++ p) caminhosPossiveis
            putStrLn "Usando banco de palavras de emergência."
            return palavrasEmergencia
  where
    -- Função para tratar erros de leitura do arquivo
    tratarErroLeitura e = do
      putStrLn $ "ERRO ao ler arquivo: " ++ show e
      putStrLn "Usando banco de palavras de emergência."
      return palavrasEmergencia
    -- Palavras de emergência usadas quando não é possível ler o arquivo
    -- Garantem que o jogo ainda possa ser jogado mesmo sem o arquivo externo
    palavrasEmergencia = ["HASKELL", "COMPUTADOR", "PROGRAMA", "FUNCIONAL"]

-- | Lê palavras de um arquivo específico.
-- Processa o conteúdo do arquivo, convertendo para maiúsculas e removendo linhas vazias.
-- Se o arquivo estiver vazio, retorna palavras de emergência.
lerPalavrasDoArquivo :: FilePath -> IO [String]
lerPalavrasDoArquivo caminho = do
  -- Lê o conteúdo completo do arquivo
  conteudo <- readFile caminho
  -- Processa o conteúdo: divide em linhas, converte para maiúsculas, remove espaços extras e linhas vazias
  let palavras = filter (not . null) (map (map toUpper . trim) (lines conteudo))
  -- Verifica se há palavras válidas no arquivo
  if null palavras
    then do
        -- Se o arquivo está vazio, exibe um aviso e usa palavras de emergência
        putStrLn $ "AVISO: O arquivo " ++ caminho ++ " está vazio. Usando banco de palavras de emergência."
        return ["HASKELL", "COMPUTADOR", "PROGRAMA", "FUNCIONAL"]
    else return palavras  -- Retorna as palavras lidas do arquivo

-- | Seleciona uma palavra aleatória do banco de palavras.
-- Esta é a função principal exposta pelo módulo, usada para obter uma palavra para o jogo.
-- Inicializa o sistema de arquivos, lê o banco de palavras e seleciona uma palavra aleatoriamente.
selecionarPalavra :: IO String
selecionarPalavra = do
  -- Garante que o arquivo de palavras existe (cria se necessário)
  inicializarArquivoPalavras
  -- Lê todas as palavras disponíveis
  palavras <- lerBancoPalavras
  -- Verifica se há palavras disponíveis
  if null palavras
    then do
      -- Se não há palavras, usa uma palavra padrão
      putStrLn "AVISO: Nenhuma palavra disponível no banco. Usando palavra padrão 'HASKELL'."
      return "HASKELL"
    else do
      -- Seleciona uma palavra aleatoriamente do banco
      let tamanhoBanco = length palavras
      indice <- randomRIO (0, tamanhoBanco - 1)  -- Gera um índice aleatório
      return (palavras !! indice)  -- Retorna a palavra no índice aleatório

-- | Remove espaços em branco no início e no fim de uma string.
-- Útil para limpar entradas do usuário e palavras lidas de arquivos.
-- Implementação funcional que aplica dropWhile duas vezes (início e fim da string).
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Limpa o terminal usando códigos de escape ANSI.
-- Esta função é usada para limpar a tela antes de mostrar novos estados do jogo,
-- melhorando a experiência visual do usuário.
limparTela :: IO ()
limparTela = do
  putStr "\ESC[2J"  -- Código ANSI para limpar toda a tela
  putStr "\ESC[H"   -- Código ANSI para posicionar o cursor no início (0,0)
  hFlush stdout     -- Força a saída imediata dos comandos anteriores