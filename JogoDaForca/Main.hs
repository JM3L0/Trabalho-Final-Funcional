-- | Módulo principal do Jogo da Forca.
-- Responsável pela interação com o usuário (operações de IO),
-- orquestração do fluxo do jogo e integração dos demais módulos.
-- Demonstra o uso da Monada IO para lidar com efeitos colaterais (entrada/saída).
module Main (main) where -- Exporta apenas a função `main`

-- Importa os módulos do próprio projeto
import Tipos          -- Tipos de dados (Jogo, EstadoJogo), Classe Exibivel
import LogicaJogo     -- Funções puras da lógica do jogo (chutarLetra, etc.)
import Utilitarios    -- Funções auxiliares (selecionarPalavra, limparTela, etc.)
-- << ALTERADO: Importa exibirRankingGeral em vez de exibirRankingMelhores
import Ranking        (lerHistoricoPartidas, lerRankingAcumulado, salvarPontuacao, exibirRankingGeral, exibirHistoricoPartidas)


-- Importa módulos padrão do Haskell
import System.IO (hFlush, stdout)
import Data.Char (toUpper)
import Data.List (nub, intersperse)
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing)

-- | Ponto de entrada principal do programa. Executa as ações de IO iniciais.
main :: IO ()
main = do
  _ <- createDirectoryIfMissing True "data"
  limparTela
  putStrLn "Bem-vindo ao Jogo da Forca em Haskell!"
  putStrLn "======================================\n"
  nomeJogador <- pedirNomeJogador
  iniciarNovoJogo nomeJogador

-- | Pede o nome do jogador e garante que não seja vazio.
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

-- | Configura e inicia um novo jogo.
iniciarNovoJogo :: String -> IO ()
iniciarNovoJogo nomeJogador = do
  limparTela
  putStrLn $ "\nOlá, " ++ nomeJogador ++ "! Preparando um novo jogo... Boa sorte!\n"
  palavra <- selecionarPalavra
  let jogoInicial = Jogo
        { palavraSecreta = palavra
        , letrasChutadas = []
        , tentativasRestantes = maxErros
        , estadoJogo = Jogando
        }
  loopJogo nomeJogador jogoInicial

-- | Loop principal do jogo.
loopJogo :: String -> Jogo -> IO ()
loopJogo nomeJogador jogo = do
  limparTela
  putStrLn $ exibir jogo
  case estadoJogo jogo of
    Ganhou -> finalizarJogo nomeJogador jogo True
    Perdeu -> finalizarJogo nomeJogador jogo False
    Jogando -> do
      putStrLn "\nDigite uma letra (A-Z) ou tente adivinhar a palavra inteira:"
      hFlush stdout
      entrada <- getLine
      processarEntrada nomeJogador jogo entrada

-- | Processa a entrada do usuário.
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

-- | Processa a tentativa de adivinhar a palavra completa.
tentarPalavra :: String -> Jogo -> String -> IO ()
tentarPalavra nomeJogador jogo palavraTentada = do
  let palavraSecretaNormalizada = removerEspacos (map toUpper (palavraSecreta jogo))
  let palavraTentadaNormalizada = removerEspacos palavraTentada
  if palavraTentadaNormalizada == palavraSecretaNormalizada
    then do
      putStrLn "\nVocê acertou a palavra!"
      let jogoGanho = jogo { letrasChutadas = nub (letrasChutadas jogo ++ filter (/= ' ') (palavraSecreta jogo))
                           , estadoJogo = Ganhou }
      putStrLn "Pressione Enter para ver o resultado final..."
      _ <- getLine
      loopJogo nomeJogador jogoGanho
    else do
      putStrLn "\nPalavra incorreta! Você perdeu uma tentativa."
      let jogoAtualizado = jogo { tentativasRestantes = tentativasRestantes jogo - 1 }
      let novoEstado = if tentativasRestantes jogoAtualizado <= 0 then Perdeu else Jogando
      putStrLn "Pressione Enter para continuar..."
      _ <- getLine
      loopJogo nomeJogador (jogoAtualizado { estadoJogo = novoEstado })

-- | Finaliza o jogo.
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
      putStrLn $ "\nPartida atual: " ++ nomeJogador ++ " marcou " ++ show pontuacao ++ " pontos (venceu - palavra: " ++ palavraSecreta jogo ++ ")"
    else do
      putStrLn $ "\nQue pena, " ++ nomeJogador ++ "! Você perdeu."
      putStrLn $ "A palavra secreta era: " ++ intersperse ' ' (palavraSecreta jogo)
      putStrLn $ "\nPartida atual: " ++ nomeJogador ++ " marcou 0 pontos (perdeu - palavra: " ++ palavraSecreta jogo ++ ")"

  -- << ALTERADO: Chama exibirRankingGeral sem argumento
  exibirRankingGeral
  exibirHistoricoPartidas 5
  jogarNovamente nomeJogador

-- | Pergunta ao jogador se ele deseja jogar novamente.
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