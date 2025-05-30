-- | Módulo principal do Jogo da Forca.
-- Responsável pela interação com o usuário (operações de IO),
-- orquestração do fluxo do jogo e integração dos demais módulos.
-- Demonstra o uso da Monada IO para lidar com efeitos colaterais (entrada/saída).
module Main (main) where -- Exporta apenas a função `main`

-- Importa os módulos do próprio projeto
import Tipos          -- Tipos de dados (Jogo, EstadoJogo), Classe Exibivel
import LogicaJogo     -- Funções puras da lógica do jogo (chutarLetra, etc.)
import Utilitarios    -- Funções auxiliares (selecionarPalavra, limparTela, etc.)
import Ranking        -- Funções para lidar com o ranking (lerRanking, salvarPontuacao, etc.)

-- Importa módulos padrão do Haskell
import System.IO (hFlush, stdout) -- Para garantir que prompts sejam exibidos antes da leitura
import Data.Char (toUpper)        -- Para normalizar entrada do usuário
import Data.List (nub, intersperse) -- Para remover letras duplicadas ao adivinhar palavra e formatar strings
import Control.Monad (unless)     -- Para condicionais mais idiomáticas

-- | Ponto de entrada principal do programa. Executa as ações de IO iniciais.
main :: IO ()
main = do
  limparTela
  putStrLn "Bem-vindo ao Jogo da Forca em Haskell!"
  putStrLn "======================================\n"
  -- Pede o nome do jogador, que será usado para salvar a pontuação
  nomeJogador <- pedirNomeJogador
  -- Inicia o primeiro jogo para o jogador
  iniciarNovoJogo nomeJogador

-- | Pede o nome do jogador e garante que não seja vazio.
-- Ação de IO que retorna o nome digitado.
pedirNomeJogador :: IO String
pedirNomeJogador = do
  putStr "Digite seu nome: "
  hFlush stdout -- Garante que "Digite seu nome: " apareça antes de `getLine` esperar
  nome <- getLine
  if null nome
    then do
      putStrLn "Nome inválido. Por favor, digite seu nome."
      pedirNomeJogador -- Chama a si mesma recursivamente se o nome for inválido
    else
      return nome

-- | Configura e inicia um novo jogo.
-- Ação de IO que seleciona uma palavra e começa o loop principal.
--
-- Argumentos:
--   nomeJogador: O nome do jogador atual.
iniciarNovoJogo :: String -> IO ()
iniciarNovoJogo nomeJogador = do
  limparTela
  putStrLn $ "\nOlá, " ++ nomeJogador ++ "! Preparando um novo jogo... Boa sorte!\n"
  -- Seleciona uma palavra aleatória do banco (ação de IO)
  palavra <- selecionarPalavra
  -- Cria o estado inicial do jogo (registro `Jogo`)
  let jogoInicial = Jogo
        { palavraSecreta = palavra
        , letrasChutadas = []
        , tentativasRestantes = maxErros -- Constante importada de Tipos
        , estadoJogo = Jogando
        }
  -- Inicia o loop principal do jogo com o estado inicial
  loopJogo nomeJogador jogoInicial

-- | Loop principal do jogo.
-- A cada iteração: limpa a tela, exibe o estado atual, verifica se o jogo acabou,
-- pede a entrada do jogador e processa a entrada, chamando a si mesma recursivamente
-- com o novo estado do jogo.
-- Demonstra o uso de `case` para tratar os diferentes `EstadoJogo`.
--
-- Argumentos:
--   nomeJogador: O nome do jogador atual.
--   jogo: O estado atual do jogo.
loopJogo :: String -> Jogo -> IO ()
loopJogo nomeJogador jogo = do
  limparTela
  -- Exibe o estado atual do jogo usando a instância `Exibivel` definida em Tipos.hs
  putStrLn $ exibir jogo

  -- Verifica o estado atual do jogo para decidir o próximo passo
  case estadoJogo jogo of
    -- Se ganhou, finaliza o jogo com mensagem de vitória
    Ganhou -> finalizarJogo nomeJogador jogo True
    -- Se perdeu, finaliza o jogo com mensagem de derrota
    Perdeu -> finalizarJogo nomeJogador jogo False
    -- Se ainda está jogando, pede a próxima entrada do usuário
    Jogando -> do
      putStrLn "\nDigite uma letra (A-Z) ou tente adivinhar a palavra inteira:"
      hFlush stdout
      entrada <- getLine
      -- Processa a entrada e continua o loop com o novo estado do jogo
      processarEntrada nomeJogador jogo entrada

-- | Processa a entrada do usuário (letra ou palavra completa).
-- Valida a entrada e chama a função apropriada (`chutarLetra` ou `tentarPalavra`).
-- Se a entrada for inválida, exibe uma mensagem e continua o loop com o mesmo estado.
--
-- Argumentos:
--   nomeJogador: O nome do jogador atual.
--   jogo: O estado atual do jogo.
--   entrada: A string digitada pelo usuário.
processarEntrada :: String -> Jogo -> String -> IO ()
processarEntrada nomeJogador jogo entrada
  -- Caso 1: Entrada vazia
  | null entrada = entradaInvalida "Entrada não pode ser vazia."
  -- Caso 2: Entrada é uma única letra válida
  | length entrada == 1 && letraValida (head entrada) = 
      -- Chama a função pura `chutarLetra` para obter o novo estado
      let novoJogo = chutarLetra (head entrada) jogo
      -- Continua o loop com o novo estado
      in loopJogo nomeJogador novoJogo
  -- Caso 3: Entrada tem mais de um caractere (tentativa de palavra)
  -- Verifica se contém apenas letras válidas ou espaços
  | length entrada > 1 && all (\c -> letraValida c || c == ' ') entrada = 
      tentarPalavra nomeJogador jogo (map toUpper entrada)
  -- Caso 4: Qualquer outra entrada é inválida
  | otherwise = entradaInvalida "Entrada inválida. Use apenas letras A-Z ou espaços."
  where
    -- Função local para tratar entradas inválidas
    entradaInvalida msg = do
        putStrLn $ "\n" ++ msg
        putStrLn "Pressione Enter para continuar..."
        _ <- getLine -- Pausa para o usuário ler a mensagem
        loopJogo nomeJogador jogo -- Continua o loop com o mesmo estado de jogo

-- | Processa a tentativa de adivinhar a palavra completa.
-- Compara a palavra tentada com a palavra secreta (ignorando espaços e caixa).
-- Atualiza o estado do jogo para Ganhou se acertou, ou decrementa as tentativas se errou.
--
-- Argumentos:
--   nomeJogador: O nome do jogador atual.
--   jogo: O estado atual do jogo.
--   palavraTentada: A palavra (já em maiúsculas) que o jogador tentou adivinhar.
tentarPalavra :: String -> Jogo -> String -> IO ()
tentarPalavra nomeJogador jogo palavraTentada = do
  -- Normaliza a palavra secreta e a tentada removendo espaços para comparação
  let palavraSecretaNormalizada = removerEspacos (map toUpper (palavraSecreta jogo))
  let palavraTentadaNormalizada = removerEspacos palavraTentada

  if palavraTentadaNormalizada == palavraSecretaNormalizada
    then do
      -- Jogador acertou a palavra!
      putStrLn "\nVocê acertou a palavra!"
      -- Atualiza o jogo para o estado Ganhou, adicionando todas as letras da palavra
      -- às letras chutadas para exibição correta no final.
      let jogoGanho = jogo { letrasChutadas = nub (letrasChutadas jogo ++ filter (/= ' ') (palavraSecreta jogo))
                           , estadoJogo = Ganhou }
      putStrLn "Pressione Enter para ver o resultado final..."
      _ <- getLine -- Pausa
      loopJogo nomeJogador jogoGanho -- Continua o loop para finalizar o jogo
    else do
      -- Jogador errou a palavra.
      putStrLn "\nPalavra incorreta! Você perdeu uma tentativa."
      -- Decrementa as tentativas.
      let jogoAtualizado = jogo { tentativasRestantes = tentativasRestantes jogo - 1 }
      -- Verifica se o jogo acabou devido à perda da tentativa.
      let novoEstado = if tentativasRestantes jogoAtualizado <= 0 then Perdeu else Jogando
      putStrLn "Pressione Enter para continuar..."
      _ <- getLine -- Pausa
      loopJogo nomeJogador (jogoAtualizado { estadoJogo = novoEstado })

-- | Finaliza o jogo.
-- Exibe o estado final, a mensagem de vitória/derrota, a pontuação (se ganhou),
-- salva a pontuação no ranking (se ganhou) e exibe o ranking.
-- Por fim, pergunta se o jogador quer jogar novamente.
--
-- Argumentos:
--   nomeJogador: O nome do jogador.
--   jogo: O estado final do jogo.
--   venceu: Boolean indicando se o jogador ganhou ou perdeu.
finalizarJogo :: String -> Jogo -> Bool -> IO ()
finalizarJogo nomeJogador jogo venceu = do
  limparTela
  -- Exibe o estado final completo do jogo
  putStrLn "\n--- Fim de Jogo ---"
  putStrLn $ exibir jogo

  -- Salva a pontuação no arquivo de ranking, tanto para vitória quanto derrota
  sucesso <- salvarPontuacao nomeJogador jogo
  unless sucesso $
      putStrLn "Aviso: Não foi possível salvar a pontuação."

  if venceu
    then do
      let pontuacao = calcularPontuacao jogo
      putStrLn $ "\nParabéns, " ++ nomeJogador ++ "! Você venceu!"
      putStrLn $ "Sua pontuação final: " ++ show pontuacao
      -- Exibe a partida recém-jogada
      putStrLn $ "\nPartida atual: " ++ nomeJogador ++ " marcou " ++ show pontuacao ++ " pontos (venceu - palavra: " ++ palavraSecreta jogo ++ ")"
    else do
      putStrLn $ "\nQue pena, " ++ nomeJogador ++ "! Você perdeu."
      putStrLn $ "A palavra secreta era: " ++ intersperse ' ' (palavraSecreta jogo)
      -- Exibe a partida recém-jogada
      putStrLn $ "\nPartida atual: " ++ nomeJogador ++ " marcou 0 pontos (perdeu - palavra: " ++ palavraSecreta jogo ++ ")"

  -- Exibe o ranking dos melhores jogadores (baseado na pontuação acumulada)
  exibirRankingMelhores 5
  -- Exibe o histórico das partidas mais recentes
  exibirHistoricoPartidas 5
  -- Pergunta se quer jogar novamente
  jogarNovamente nomeJogador

-- | Pergunta ao jogador se ele deseja jogar novamente.
-- Se sim, inicia um novo jogo; se não, encerra o programa.
--
-- Argumentos:
--   nomeJogador: O nome do jogador (para iniciar um novo jogo se ele aceitar).
jogarNovamente :: String -> IO ()
jogarNovamente nomeJogador = do
  putStr "Deseja jogar novamente? (S/N): "
  hFlush stdout
  resposta <- getLine
  case map toUpper resposta of
    "S" -> iniciarNovoJogo nomeJogador -- Chama a função para iniciar um novo jogo
    "N" -> putStrLn "\nObrigado por jogar! Até a próxima."
    _   -> do
      putStrLn "Resposta inválida. Por favor, digite S para Sim ou N para Não."
      jogarNovamente nomeJogador -- Pede a resposta novamente