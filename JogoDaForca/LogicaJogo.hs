-- LogicaJogo.hs
-- | Contém a lógica pura do Jogo da Forca, separada da interação com IO.
module LogicaJogo (
    chutarLetra,
    letraValida,
    removerEspacos,
    calcularPontuacao
) where

import Tipos ( Jogo, EstadoJogo(..), maxErros, -- Importa o tipo Jogo (abstrato)
               palavraSecretaJogo, letrasChutadasJogo, tentativasRestantesJogo, estadoJogoJogo, -- Getters
               construirJogoComNovosValores ) -- Função para construir Jogo
import Data.Char (toUpper)
import Data.List (nub)

-- | Processa o chute de uma letra pelo jogador, atualizando o estado do jogo.
chutarLetra :: Char -> Jogo -> Jogo
chutarLetra letra jogo
  | letraNormalizada `elem` letrasChutadasJogo jogo = jogo
  | otherwise =
      let ps = palavraSecretaJogo jogo
          lcAtuais = letrasChutadasJogo jogo
          trAtuais = tentativasRestantesJogo jogo

          novasLetrasChutadas = nub (letraNormalizada : lcAtuais)
          acertou = letraNormalizada `elem` map toUpper ps
          novasTentativas = if acertou then trAtuais else trAtuais - 1
          
          novoEstado = atualizarEstadoJogo ps novasTentativas novasLetrasChutadas
      in construirJogoComNovosValores ps novasLetrasChutadas novasTentativas novoEstado
  where
    letraNormalizada = toUpper letra

-- | Atualiza o estado do jogo (Jogando, Ganhou, Perdeu) após um chute.
atualizarEstadoJogo :: String -> Int -> [Char] -> EstadoJogo
atualizarEstadoJogo palavraSecretaDaVez novasTentativas novasLetrasChutadas
  | all (\c -> c == ' ' || toUpper c `elem` novasLetrasChutadasNormalizadas) palavraSecretaDaVez = Ganhou
  | novasTentativas <= 0 = Perdeu
  | otherwise = Jogando
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
      let pontuacaoBase = tentativasRestantesJogo jogo * 10
          letrasCorretas = filter (\c -> toUpper c `elem` map toUpper (palavraSecretaJogo jogo)) (letrasChutadasJogo jogo)
          letrasUnicasCorretas = nub letrasCorretas
          bonusLetrasUnicas = length letrasUnicasCorretas * 5
      in pontuacaoBase + bonusLetrasUnicas
  | otherwise = 0