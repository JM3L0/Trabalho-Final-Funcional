-- Main.hs
-- | Módulo principal do Jogo da Forca.
-- Este módulo coordena todo o fluxo do jogo, desde a inicialização
-- até o término de cada partida, interagindo com o usuário via terminal.
module Main (main) where

import Tipos          ( Jogo, EstadoJogo(..), Exibivel(..), maxErros,
                        criarJogoInicial,
                        palavraSecretaJogo, letrasChutadasJogo, tentativasRestantesJogo, estadoJogoJogo,
                        construirJogoComNovosValores )
import LogicaJogo     ( chutarLetra, letraValida, removerEspacos, calcularPontuacao )
import Utilitarios    ( selecionarPalavra, limparTela, trim )
import Ranking        ( salvarPontuacao, exibirRankingGeral, exibirHistoricoPartidas )

import System.IO (hFlush, stdout)  -- Para manipulação de I/O
import Data.Char (toUpper)         -- Para conversão de caracteres
import Data.List (nub, intersperse)  -- Para manipulação de listas
import Control.Monad (unless)      -- Para operações condicionais em monads
import System.Directory (createDirectoryIfMissing)  -- Para criar diretórios

-- | Função principal que inicia o jogo
-- Esta é a função de entrada do programa. Ela inicializa o ambiente,
-- cria a estrutura de diretórios necessária e começa o fluxo do jogo.
main :: IO ()
main = do
  -- Cria o diretório 'data' caso não exista
  -- Este diretório será usado para armazenar arquivos de palavras e ranking
  _ <- createDirectoryIfMissing True "data"
  
  -- Limpa a tela e exibe mensagem de boas-vindas
  limparTela
  putStrLn "Bem-vindo ao Jogo da Forca em Haskell!"
  putStrLn "======================================\n"
  
  -- Solicita o nome do jogador
  nomeJogador <- pedirNomeJogador
  
  -- Inicia o jogo com o nome fornecido
  iniciarNovoJogo nomeJogador

-- | Solicita e valida o nome do jogador
-- Pede ao usuário que informe seu nome e valida a entrada.
-- O nome não pode ser vazio.
pedirNomeJogador :: IO String
pedirNomeJogador = do
  putStr "Digite seu nome: "
  hFlush stdout  -- Garante que o prompt é exibido imediatamente
  nome <- getLine  -- Lê a entrada do usuário
  
  -- Verifica se o nome é válido (não vazio)
  if null nome
    then do
      putStrLn "Nome inválido. Por favor, digite seu nome."
      pedirNomeJogador  -- Recursão até obter um nome válido
    else
      return nome  -- Retorna o nome válido

-- | Inicia um novo jogo com o nome do jogador fornecido
-- Seleciona uma palavra aleatória e cria um novo estado de jogo.
iniciarNovoJogo :: String -> IO ()
iniciarNovoJogo nomeJogador = do
  limparTela  -- Limpa a tela para começar com uma interface limpa
  putStrLn $ "\nOlá, " ++ nomeJogador ++ "! Preparando um novo jogo... Boa sorte!\n"
  
  -- Seleciona uma palavra aleatória do banco de palavras
  palavra <- selecionarPalavra
  
  -- Cria um novo estado de jogo com a palavra selecionada
  let jogoInicial = criarJogoInicial palavra
  
  -- Inicia o loop principal do jogo
  loopJogo nomeJogador jogoInicial

-- | Loop principal do jogo que processa cada rodada
-- Esta função gerencia o fluxo do jogo, exibindo o estado atual,
-- verificando se o jogo terminou e processando as entradas do jogador.
loopJogo :: String -> Jogo -> IO ()
loopJogo nomeJogador jogo = do
  limparTela  -- Limpa a tela para atualizar a interface
  putStrLn $ exibir jogo  -- Exibe o estado atual do jogo (forca, palavra, letras usadas)
  
  -- Verifica o estado do jogo
  case estadoJogoJogo jogo of
    Ganhou -> finalizarJogo nomeJogador jogo True   -- O jogador ganhou
    Perdeu -> finalizarJogo nomeJogador jogo False  -- O jogador perdeu
    Jogando -> do  -- O jogo continua
      -- Solicita a próxima entrada do jogador
      putStrLn "\nDigite uma letra (A-Z) ou tente adivinhar a palavra inteira:"
      hFlush stdout  -- Garante que o prompt é exibido imediatamente
      entrada <- getLine  -- Lê a entrada do usuário
      
      -- Processa a entrada do jogador
      processarEntrada nomeJogador jogo entrada

-- | Processa a entrada do jogador (letra única ou palavra completa)
-- Analisa a entrada do usuário e atualiza o estado do jogo de acordo.
-- Aceita tanto uma única letra quanto uma tentativa de palavra completa.
processarEntrada :: String -> Jogo -> String -> IO ()
processarEntrada nomeJogador jogo entrada = case entrada of
    -- Caso 1: Entrada vazia
    [] -> entradaInvalida "Entrada não pode ser vazia."
    
    -- Caso 2: Uma única letra
    [letraChutada] ->
      if letraValida letraChutada then
          -- Se a letra é válida, processa o chute
          let novoJogo = chutarLetra letraChutada jogo
          in loopJogo nomeJogador novoJogo
      else
          -- Se a letra é inválida, exibe mensagem de erro
          entradaInvalida "Letra inválida. Use apenas A-Z."
    
    -- Caso 3: Múltiplos caracteres (tentativa de palavra)
    multiCharString ->
      if all (\c -> letraValida c || c == ' ') multiCharString then
          -- Se todos os caracteres são válidos, tenta adivinhar a palavra
          tentarPalavra nomeJogador jogo (map toUpper multiCharString)
      else
          -- Se há caracteres inválidos, exibe mensagem de erro
          entradaInvalida "Tentativa de palavra inválida. Use apenas letras A-Z ou espaços."
  where
    -- Função auxiliar para tratar entradas inválidas
    entradaInvalida msg = do
        putStrLn $ "\n" ++ msg
        putStrLn "Pressione Enter para continuar..."
        _ <- getLine
        loopJogo nomeJogador jogo  -- Volta ao loop principal sem alterar o estado

-- | Processa a tentativa de adivinhar a palavra completa
-- Compara a palavra tentada com a palavra secreta e atualiza o estado do jogo.
tentarPalavra :: String -> Jogo -> String -> IO ()
tentarPalavra nomeJogador jogo palavraTentada = do
  -- Obtém os valores atuais do jogo
  let psJogo = palavraSecretaJogo jogo         -- Palavra secreta
  let lcJogo = letrasChutadasJogo jogo         -- Letras já chutadas
  let trJogo = tentativasRestantesJogo jogo    -- Tentativas restantes
  
  -- Normaliza as palavras para comparação (remove espaços e converte para maiúsculas)
  let palavraSecretaNormalizada = removerEspacos (map toUpper psJogo)
  let palavraTentadaNormalizada = removerEspacos palavraTentada

  -- Verifica se a tentativa está correta
  if palavraTentadaNormalizada == palavraSecretaNormalizada
    then do
      -- Acertou a palavra!
      putStrLn "\nVocê acertou a palavra!"
      
      -- Adiciona todas as letras da palavra às letras chutadas para mostrar corretamente no final
      -- Usa nub para remover duplicatas
      let novasLetrasChutadasParaGanho = nub (lcJogo ++ filter (/= ' ') psJogo)
      
      -- Cria um novo estado de jogo com o estado "Ganhou"
      let jogoGanho = construirJogoComNovosValores
                        psJogo
                        novasLetrasChutadasParaGanho
                        trJogo
                        Ganhou
      
      putStrLn "Pressione Enter para ver o resultado final..."
      _ <- getLine
      
      -- Continua o loop com o novo estado (que vai finalizar o jogo na próxima iteração)
      loopJogo nomeJogador jogoGanho
    else do
      -- Errou a palavra
      putStrLn "\nPalavra incorreta! Você perdeu uma tentativa."
      
      -- Reduz o número de tentativas
      let novasTentativas = trJogo - 1
      
      -- Determina se o jogador perdeu (sem mais tentativas)
      let novoEstadoParaDerrota = if novasTentativas <= 0 then Perdeu else Jogando
      
      -- Cria um novo estado de jogo com as tentativas reduzidas
      let jogoAtualizado = construirJogoComNovosValores
                             psJogo
                             lcJogo
                             novasTentativas
                             novoEstadoParaDerrota
      
      putStrLn "Pressione Enter para continuar..."
      _ <- getLine
      
      -- Continua o loop com o novo estado
      loopJogo nomeJogador jogoAtualizado

-- | Finaliza o jogo, exibindo o resultado e salvando a pontuação
-- Esta função é chamada quando o jogo termina (vitória ou derrota).
-- Exibe o resultado final, salva a pontuação e pergunta se o jogador quer jogar novamente.
finalizarJogo :: String -> Jogo -> Bool -> IO ()
finalizarJogo nomeJogador jogo venceu = do
  limparTela  -- Limpa a tela para exibir o resultado final
  putStrLn "\n--- Fim de Jogo ---"
  putStrLn $ exibir jogo  -- Exibe o estado final do jogo
  
  -- Salva a pontuação no ranking
  sucesso <- salvarPontuacao nomeJogador jogo
  unless sucesso $
      putStrLn "Aviso: Não foi possível salvar a pontuação."

  -- Exibe mensagem diferente dependendo do resultado
  if venceu
    then do
      -- Jogador venceu
      let pontuacao = calcularPontuacao jogo  -- Calcula a pontuação final
      putStrLn $ "\nParabéns, " ++ nomeJogador ++ "! Você venceu!"
      putStrLn $ "Sua pontuação final: " ++ show pontuacao
      putStrLn $ "\nPartida atual: " ++ nomeJogador ++ " marcou " ++ show pontuacao ++ " pontos (venceu - palavra: " ++ palavraSecretaJogo jogo ++ ")"
    else do
      -- Jogador perdeu
      putStrLn $ "\nQue pena, " ++ nomeJogador ++ "! Você perdeu."
      putStrLn $ "A palavra secreta era: " ++ intersperse ' ' (palavraSecretaJogo jogo)
      putStrLn $ "\nPartida atual: " ++ nomeJogador ++ " marcou 0 pontos (perdeu - palavra: " ++ palavraSecretaJogo jogo ++ ")"

  -- Exibe o ranking geral e o histórico de partidas recentes
  exibirRankingGeral            -- Mostra o ranking de todos os jogadores
  exibirHistoricoPartidas 5     -- Mostra as 5 partidas mais recentes
  
  -- Pergunta se quer jogar novamente
  jogarNovamente nomeJogador

-- | Pergunta ao jogador se deseja jogar novamente
-- Processa a resposta do jogador e inicia um novo jogo ou encerra o programa.
jogarNovamente :: String -> IO ()
jogarNovamente nomeJogador = do
  putStr "Deseja jogar novamente? (S/N): "
  hFlush stdout  -- Garante que o prompt é exibido imediatamente
  resposta <- getLine  -- Lê a resposta do usuário
  
  -- Processa a resposta (não diferencia maiúsculas/minúsculas)
  case map toUpper resposta of
    "S" -> iniciarNovoJogo nomeJogador  -- Inicia um novo jogo
    "N" -> putStrLn "\nObrigado por jogar! Até a próxima."  -- Encerra o programa
    _   -> do
      -- Resposta inválida, solicita novamente
      putStrLn "Resposta inválida. Por favor, digite S para Sim ou N para Não."
      jogarNovamente nomeJogador  -- Recursão até obter uma resposta válida