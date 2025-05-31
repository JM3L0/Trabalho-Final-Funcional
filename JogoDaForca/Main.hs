-- Main.hs
-- | Módulo principal do Jogo da Forca.
module Main (main) where

import Tipos          ( Jogo, EstadoJogo(..), Exibivel(..), maxErros,
                        criarJogoInicial,
                        palavraSecretaJogo, letrasChutadasJogo, tentativasRestantesJogo, estadoJogoJogo,
                        construirJogoComNovosValores )
import LogicaJogo     ( chutarLetra, letraValida, removerEspacos, calcularPontuacao )
import Utilitarios    ( selecionarPalavra, limparTela, trim )
import Ranking        ( salvarPontuacao, exibirRankingGeral, exibirHistoricoPartidas )

import System.IO (hFlush, stdout)
import Data.Char (toUpper)
import Data.List (nub, intersperse)
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  _ <- createDirectoryIfMissing True "data"
  limparTela
  putStrLn "Bem-vindo ao Jogo da Forca em Haskell!"
  putStrLn "======================================\n"
  nomeJogador <- pedirNomeJogador
  iniciarNovoJogo nomeJogador

pedirNomeJogador :: IO String
pedirNomeJogador = do
  putStr "Digite seu nome: "
  hFlush stdout
  nome <- getLine
  if null nome
    then do
      putStrLn "Nome inválido. Por favor, digite seu nome."
      pedirNomeJogador
    else
      return nome

iniciarNovoJogo :: String -> IO ()
iniciarNovoJogo nomeJogador = do
  limparTela
  putStrLn $ "\nOlá, " ++ nomeJogador ++ "! Preparando um novo jogo... Boa sorte!\n"
  palavra <- selecionarPalavra
  let jogoInicial = criarJogoInicial palavra
  loopJogo nomeJogador jogoInicial

loopJogo :: String -> Jogo -> IO ()
loopJogo nomeJogador jogo = do
  limparTela
  putStrLn $ exibir jogo
  case estadoJogoJogo jogo of
    Ganhou -> finalizarJogo nomeJogador jogo True
    Perdeu -> finalizarJogo nomeJogador jogo False
    Jogando -> do
      putStrLn "\nDigite uma letra (A-Z) ou tente adivinhar a palavra inteira:"
      hFlush stdout
      entrada <- getLine
      processarEntrada nomeJogador jogo entrada

processarEntrada :: String -> Jogo -> String -> IO ()
processarEntrada nomeJogador jogo entrada = case entrada of
    [] -> entradaInvalida "Entrada não pode ser vazia."
    [letraChutada] ->
      if letraValida letraChutada then
          let novoJogo = chutarLetra letraChutada jogo
          in loopJogo nomeJogador novoJogo
      else
          entradaInvalida "Letra inválida. Use apenas A-Z."
    multiCharString ->
      if all (\c -> letraValida c || c == ' ') multiCharString then
          tentarPalavra nomeJogador jogo (map toUpper multiCharString)
      else
          entradaInvalida "Tentativa de palavra inválida. Use apenas letras A-Z ou espaços."
  where
    entradaInvalida msg = do
        putStrLn $ "\n" ++ msg
        putStrLn "Pressione Enter para continuar..."
        _ <- getLine
        loopJogo nomeJogador jogo

tentarPalavra :: String -> Jogo -> String -> IO ()
tentarPalavra nomeJogador jogo palavraTentada = do
  let psJogo = palavraSecretaJogo jogo
  let lcJogo = letrasChutadasJogo jogo
  let trJogo = tentativasRestantesJogo jogo
  
  let palavraSecretaNormalizada = removerEspacos (map toUpper psJogo)
  let palavraTentadaNormalizada = removerEspacos palavraTentada

  if palavraTentadaNormalizada == palavraSecretaNormalizada
    then do
      putStrLn "\nVocê acertou a palavra!"
      let novasLetrasChutadasParaGanho = nub (lcJogo ++ filter (/= ' ') psJogo)
      let jogoGanho = construirJogoComNovosValores
                        psJogo
                        novasLetrasChutadasParaGanho
                        trJogo
                        Ganhou
      putStrLn "Pressione Enter para ver o resultado final..."
      _ <- getLine
      loopJogo nomeJogador jogoGanho
    else do
      putStrLn "\nPalavra incorreta! Você perdeu uma tentativa."
      let novasTentativas = trJogo - 1
      let novoEstadoParaDerrota = if novasTentativas <= 0 then Perdeu else Jogando
      let jogoAtualizado = construirJogoComNovosValores
                             psJogo
                             lcJogo
                             novasTentativas
                             novoEstadoParaDerrota
      putStrLn "Pressione Enter para continuar..."
      _ <- getLine
      loopJogo nomeJogador jogoAtualizado

finalizarJogo :: String -> Jogo -> Bool -> IO ()
finalizarJogo nomeJogador jogo venceu = do
  limparTela
  putStrLn "\n--- Fim de Jogo ---"
  putStrLn $ exibir jogo
  sucesso <- salvarPontuacao nomeJogador jogo
  unless sucesso $
      putStrLn "Aviso: Não foi possível salvar a pontuação."

  if venceu
    then do
      let pontuacao = calcularPontuacao jogo
      putStrLn $ "\nParabéns, " ++ nomeJogador ++ "! Você venceu!"
      putStrLn $ "Sua pontuação final: " ++ show pontuacao
      putStrLn $ "\nPartida atual: " ++ nomeJogador ++ " marcou " ++ show pontuacao ++ " pontos (venceu - palavra: " ++ palavraSecretaJogo jogo ++ ")"
    else do
      putStrLn $ "\nQue pena, " ++ nomeJogador ++ "! Você perdeu."
      putStrLn $ "A palavra secreta era: " ++ intersperse ' ' (palavraSecretaJogo jogo)
      putStrLn $ "\nPartida atual: " ++ nomeJogador ++ " marcou 0 pontos (perdeu - palavra: " ++ palavraSecretaJogo jogo ++ ")"

  exibirRankingGeral
  exibirHistoricoPartidas 5
  jogarNovamente nomeJogador

jogarNovamente :: String -> IO ()
jogarNovamente nomeJogador = do
  putStr "Deseja jogar novamente? (S/N): "
  hFlush stdout
  resposta <- getLine
  case map toUpper resposta of
    "S" -> iniciarNovoJogo nomeJogador
    "N" -> putStrLn "\nObrigado por jogar! Até a próxima."
    _   -> do
      putStrLn "Resposta inválida. Por favor, digite S para Sim ou N para Não."
      jogarNovamente nomeJogador