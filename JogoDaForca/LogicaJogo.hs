-- LogicaJogo.hs
-- | Contém a lógica pura do Jogo da Forca, separada da interação com IO.
module LogicaJogo (
    chutarLetra,        -- Processa o chute de uma letra
    letraValida,        -- Verifica se uma letra é válida
    removerEspacos,     -- Remove espaços de uma string
    calcularPontuacao   -- Calcula a pontuação final
) where

import Tipos ( Jogo, EstadoJogo(..), maxErros, -- Importa o tipo Jogo (abstrato)
               palavraSecretaJogo, letrasChutadasJogo, tentativasRestantesJogo, estadoJogoJogo, -- Getters
               construirJogoComNovosValores ) -- Função para construir Jogo
import Data.Char (toUpper)
import Data.List (nub)

-- | Processa o chute de uma letra pelo jogador, atualizando o estado do jogo.
chutarLetra :: Char -> Jogo -> Jogo
chutarLetra letra jogo
  | letraNormalizada `elem` letrasChutadasJogo jogo = jogo  -- Letra já foi chutada, retorna o jogo sem alterações
  | otherwise =
      let ps = palavraSecretaJogo jogo
          lcAtuais = letrasChutadasJogo jogo
          trAtuais = tentativasRestantesJogo jogo

          novasLetrasChutadas = nub (letraNormalizada : lcAtuais)  -- Adiciona a nova letra à lista de chutadas
          acertou = letraNormalizada `elem` map toUpper ps  -- Verifica se a letra está na palavra
          novasTentativas = if acertou then trAtuais else trAtuais - 1  -- Reduz tentativas se errou
          
          novoEstado = atualizarEstadoJogo ps novasTentativas novasLetrasChutadas  -- Calcula novo estado
      in construirJogoComNovosValores ps novasLetrasChutadas novasTentativas novoEstado
  where
    letraNormalizada = toUpper letra  -- Normaliza a letra para maiúscula

-- | Atualiza o estado do jogo (Jogando, Ganhou, Perdeu) após um chute.
atualizarEstadoJogo :: String -> Int -> [Char] -> EstadoJogo
atualizarEstadoJogo palavraSecretaDaVez novasTentativas novasLetrasChutadas
  | all (\c -> c == ' ' || toUpper c `elem` novasLetrasChutadasNormalizadas) palavraSecretaDaVez = Ganhou  -- Todas as letras foram descobertas
  | novasTentativas <= 0 = Perdeu  -- Acabaram as tentativas
  | otherwise = Jogando  -- Jogo continua
  where
    novasLetrasChutadasNormalizadas = map toUpper novasLetrasChutadas

-- | Verifica se um caractere é uma letra válida para o chute (A-Z).
letraValida :: Char -> Bool
letraValida c = toUpper c `elem` ['A'..'Z']

-- | Remove espaços de uma string.
removerEspacos :: String -> String
removerEspacos = filter (/= ' ')

-- | Calcula a pontuação final do jogador.
calcularPontuacao :: Jogo -> Int
calcularPontuacao jogo
  | estadoJogoJogo jogo == Ganhou =
      let pontuacaoBase = tentativasRestantesJogo jogo * 10  -- 10 pontos por tentativa restante
          letrasCorretas = filter (\c -> toUpper c `elem` map toUpper (palavraSecretaJogo jogo)) (letrasChutadasJogo jogo)
          letrasUnicasCorretas = nub letrasCorretas  -- Remove duplicatas
          bonusLetrasUnicas = length letrasUnicasCorretas * 5  -- 5 pontos por letra única correta
      in pontuacaoBase + bonusLetrasUnicas
  | otherwise = 0  -- Sem pontuação se não ganhou