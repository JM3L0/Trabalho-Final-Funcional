-- | Contém a lógica pura do Jogo da Forca, separada da interação com IO.
-- Este módulo demonstra funções puras, pattern matching, guards e manipulação de listas.
module LogicaJogo (
    -- * Funções Principais da Lógica
    chutarLetra,        -- ^ Processa um chute de letra, atualizando o estado do jogo.
    letraValida,        -- ^ Verifica se um caractere é uma letra válida para chute.
    removerEspacos,     -- ^ Remove espaços de uma string.
    calcularPontuacao   -- ^ Calcula a pontuação final do jogador.
) where

import Tipos
import Data.Char (toUpper)
import Data.List (nub)

-- | Processa o chute de uma letra pelo jogador, atualizando o estado do jogo.
-- Função pura: recebe um estado e uma entrada, retorna um novo estado sem efeitos colaterais.
-- Demonstra Pattern Matching na guarda e `let` para definições locais.
--
-- Argumentos:
--   letra: O caractere chutado pelo jogador.
--   jogo: O estado atual do jogo.
--
-- Retorna:
--   O novo estado do jogo após processar o chute.
chutarLetra :: Char -> Jogo -> Jogo
chutarLetra letra jogo
  -- Guarda (Guard): Se a letra (normalizada para maiúscula) já foi chutada, retorna o jogo sem modificação.
  | letraNormalizada `elem` letrasChutadas jogo = jogo
  -- Caso contrário, calcula o novo estado.
  | otherwise = 
      let -- Definições locais usando `let`:
          -- Adiciona a nova letra (normalizada) à lista de letras chutadas, removendo duplicatas com `nub`.
          novasLetrasChutadas = nub (letraNormalizada : letrasChutadas jogo)
          -- Verifica se a letra chutada pertence à palavra secreta (normalizada).
          acertou = letraNormalizada `elem` palavraSecretaNormalizada
          -- Atualiza o número de tentativas restantes.
          novasTentativas = if acertou then tentativasRestantes jogo else tentativasRestantes jogo - 1
          -- Determina o novo estado do jogo (Jogando, Ganhou, Perdeu).
          novoEstado = atualizarEstadoJogo jogo letraNormalizada novasTentativas novasLetrasChutadas
      -- Retorna um novo registro `Jogo` com os campos atualizados.
      in jogo { letrasChutadas = novasLetrasChutadas
              , tentativasRestantes = novasTentativas
              , estadoJogo = novoEstado
              }
  where
    -- Normaliza a letra chutada e a palavra secreta para maiúsculas para comparação case-insensitive.
    letraNormalizada = toUpper letra
    palavraSecretaNormalizada = map toUpper (palavraSecreta jogo)

-- | Atualiza o estado do jogo (Jogando, Ganhou, Perdeu) após um chute.
-- Função auxiliar pura, usada por `chutarLetra`.
-- Demonstra o uso de `all` (função de ordem superior) e Guardas.
--
-- Argumentos:
--   jogo: O estado do jogo *antes* do chute atual ser totalmente processado (usado para obter a palavra secreta).
--   letraChutada: A letra que acabou de ser chutada (já normalizada).
--   novasTentativas: O número de tentativas restantes *após* o chute.
--   novasLetrasChutadas: A lista de letras chutadas *incluindo* a letra atual.
--
-- Retorna:
--   O novo `EstadoJogo`.
atualizarEstadoJogo :: Jogo -> Char -> Int -> [Char] -> EstadoJogo
atualizarEstadoJogo jogo _ novasTentativas novasLetrasChutadas
  -- Guarda 1: Verifica se todas as letras da palavra secreta (exceto espaços) estão na lista de letras chutadas.
  -- Usa `all`, uma função de ordem superior, para verificar a condição para cada caractere.
  | all (\c -> c == ' ' || toUpper c `elem` novasLetrasChutadasNormalizadas) (palavraSecreta jogo) = Ganhou
  -- Guarda 2: Verifica se as tentativas acabaram.
  | novasTentativas <= 0 = Perdeu
  -- Caso contrário (Default): O jogo continua.
  | otherwise = Jogando
  where
    -- Normaliza as letras chutadas para comparação.
    novasLetrasChutadasNormalizadas = map toUpper novasLetrasChutadas

-- | Verifica se um caractere é uma letra válida para o chute (A-Z).
-- Função pura simples.
letraValida :: Char -> Bool
letraValida c = toUpper c `elem` ['A'..'Z']

-- | Remove espaços de uma string.
-- Função pura que demonstra o uso de `filter` (função de ordem superior).
removerEspacos :: String -> String
removerEspacos = filter (/= ' ')

-- | Calcula a pontuação final do jogador.
-- Função pura que demonstra Guardas e `let`.
-- A pontuação só é calculada se o jogador ganhou.
--
-- Argumentos:
--   jogo: O estado final do jogo.
--
-- Retorna:
--   A pontuação calculada (Int).
calcularPontuacao :: Jogo -> Int
calcularPontuacao jogo
  -- Guarda: Só calcula a pontuação se o estado for Ganhou.
  | estadoJogo jogo == Ganhou = 
      let -- Pontuação base: 10 pontos por tentativa restante.
          pontuacaoBase = tentativasRestantes jogo * 10
          -- Bônus: 5 pontos por cada letra *única* correta chutada.
          letrasCorretas = filter (\c -> toUpper c `elem` palavraSecretaNormalizada) (letrasChutadas jogo)
          letrasUnicasCorretas = nub letrasCorretas
          bonusLetrasUnicas = length letrasUnicasCorretas * 5
      in pontuacaoBase + bonusLetrasUnicas
  -- Caso contrário (Perdeu ou ainda Jogando, embora não deva chegar aqui no fluxo normal), a pontuação é 0.
  | otherwise = 0
  where
    palavraSecretaNormalizada = map toUpper (palavraSecreta jogo)

