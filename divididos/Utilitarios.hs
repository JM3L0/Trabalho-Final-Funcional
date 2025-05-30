-- | Funções utilitárias diversas para o Jogo da Forca.
-- Este módulo agrupa funcionalidades auxiliares como o banco de palavras,
-- seleção aleatória de palavras e limpeza da tela do console.
module Utilitarios (
    -- * Banco de Palavras
    bancoPalavrasPadrao, -- ^ Lista de palavras padrão (fallback).
    selecionarPalavra,  -- ^ Seleciona uma palavra aleatória do banco.
    arquivoPalavras,    -- ^ Caminho do arquivo de palavras.
    -- * Funções de Terminal
    limparTela          -- ^ Limpa a tela do console.
) where

import System.IO (hFlush, stdout, readFile)
-- Importa System.Random para uma melhor geração de números aleatórios.
import System.Random (randomRIO)
-- Importações adicionais para tratamento de erros e manipulação de strings
import System.IO.Error (catchIOError, isDoesNotExistError)
import Data.Char (toUpper, isSpace)
import Control.Exception (try)

-- | Caminho para o arquivo que contém as palavras do jogo.
-- Usa o mesmo padrão de diretório do arquivo de ranking.
arquivoPalavras :: FilePath
arquivoPalavras = "../data/palavras.txt"

-- | Banco de palavras padrão para o jogo (usado como fallback).
-- Utilizado quando o arquivo de palavras não pode ser lido.
bancoPalavrasPadrao :: [String]
bancoPalavrasPadrao = ["HASKELL", "PROGRAMACAO", "FUNCIONAL", "MONADA", "TIPO", "LAZINESS", "CURRY", "PADRAO", "RECURSAO", "GUARDAS", "LISTA",
                 "FUNCAO", "ALGORITMO", "DADOS", "TIPOS", "FUNCOES", "LAMBDA", "RECURSIVIDADE", "JULIO", "MARCAO", "HERMERSON", "RAILDOM", "JOAO MARCOS",
                 "COMPUTADOR", "COMPILADOR", "INTERPRETADOR", "LINGUAGEM", "MEMORIA", "PROCESSADOR", "SEMANTICA",
                 "VARIAVEL", "CONSTANTE", "BIBLIOTECA", "BINARIO", "FRAMEWORK", "ITERACAO", "PARADIGMA",
                 "INTERFACE", "TERMINAL", "LOGICA", "METODO", "OBJETO", "INSTANCIA", "ESCOPO", "MODULO",
                 "PARAMETRO", "RETORNO", "CLASSE", "HERANCA", "ATRIBUTO", "SOFTWARE", "HARDWARE", "INTERNET"
                ]

-- | Função auxiliar para remover espaços em branco no início e fim de uma string.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Lê o banco de palavras do arquivo, com tratamento de erros.
-- Se o arquivo não existir ou ocorrer outro erro, retorna o banco padrão.
lerBancoPalavras :: IO [String]
lerBancoPalavras = catchIOError lerPalavrasInterno tratarErroLeitura
  where
    tratarErroLeitura e =
      if isDoesNotExistError e
        then do
          putStrLn $ "Arquivo de palavras (" ++ arquivoPalavras ++ ") não encontrado. Usando banco padrão."
          return bancoPalavrasPadrao
        else do
          putStrLn $ "Erro ao ler o arquivo de palavras: " ++ show e
          return bancoPalavrasPadrao

-- | Função interna que realiza a leitura e processamento do arquivo de palavras.
lerPalavrasInterno :: IO [String]
lerPalavrasInterno = do
  conteudo <- readFile arquivoPalavras
  -- Converte cada linha para maiúsculas, remove espaços extras e filtra linhas vazias
  let palavras = filter (not . null) (map (map toUpper . trim) (lines conteudo))
  -- Se o arquivo estiver vazio, usa o banco padrão
  if null palavras
    then return bancoPalavrasPadrao
    else return palavras

-- | Seleciona uma palavra aleatória do banco de palavras (lido do arquivo).
-- Demonstra uma ação de IO para obter aleatoriedade.
--
-- Retorna:
--   IO String: Uma ação de IO que resulta na palavra selecionada.
selecionarPalavra :: IO String
selecionarPalavra = do
  -- Primeiro lê o banco de palavras do arquivo (com tratamento de erro)
  palavras <- lerBancoPalavras
  let tamanhoBanco = length palavras
  -- Gera um índice aleatório entre 0 e (tamanhoBanco - 1).
  indice <- randomRIO (0, tamanhoBanco - 1)
  -- Retorna a palavra no índice selecionado.
  return (palavras !! indice)

-- | Limpa a tela do terminal e posiciona o cursor no início.
-- Demonstra uma ação de IO que interage com o terminal.
-- Usa sequências de escape ANSI padrão para compatibilidade com a maioria dos terminais.
limparTela :: IO ()
limparTela = do
  putStr "\ESC[2J"  -- Código ANSI para limpar a tela (Clear Screen).
  putStr "\ESC[H"   -- Código ANSI para mover o cursor para a posição Home (topo esquerdo).
  hFlush stdout    -- Garante que os comandos de limpeza sejam enviados imediatamente ao terminal.

