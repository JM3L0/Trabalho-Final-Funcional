-- Tipos.hs
module Tipos (
    -- * Tipos Principais
    Jogo,               -- Exporta apenas o TIPO Jogo, não seus construtores/campos internos
    EstadoJogo(..),     -- Exporta o tipo e todos seus construtores
    -- * Classe Exibivel
    Exibivel(..),       -- Exporta a classe Exibivel e seu método
    -- * Constantes
    maxErros,           -- Número máximo de erros permitidos
    -- * Interface para o tipo Jogo
    criarJogoInicial,   -- Cria um novo jogo
    palavraSecretaJogo, -- Obtém a palavra secreta
    letrasChutadasJogo, -- Obtém as letras já chutadas
    tentativasRestantesJogo, -- Obtém tentativas restantes
    estadoJogoJogo,     -- Obtém o estado atual do jogo
    -- Função para módulos externos construírem um novo estado de Jogo
    construirJogoComNovosValores -- Constrói um novo jogo com valores específicos
) where

import Data.List (intersperse)
import Data.Char (toUpper)

-- | O número máximo de erros permitidos antes de o jogador perder.
maxErros :: Int
maxErros = 6

-- | Enumeração dos possíveis estados do jogo.
data EstadoJogo = Jogando  -- O jogo está em andamento
                | Ganhou   -- O jogador ganhou
                | Perdeu   -- O jogador perdeu
                deriving (Eq, Show)

-- | Tipo de dados principal que representa o estado do jogo.
-- O construtor de dados `JogoInternal` e os nomes dos campos (prefixados com 'i')
-- não são exportados, tornando `Jogo` um tipo abstrato para módulos externos.
data Jogo = JogoInternal {
    iPalavraSecreta :: String,      -- A palavra que o jogador deve adivinhar
    iLetrasChutadas :: [Char],      -- Letras que o jogador já tentou
    iTentativasRestantes :: Int,    -- Número de erros permitidos antes de perder
    iEstadoJogo :: EstadoJogo       -- Estado atual do jogo
} deriving (Show)

-- --- Funções de Interface para o tipo Jogo (Exportadas) ---

-- | Cria um estado inicial para o jogo com uma palavra secreta.
criarJogoInicial :: String -> Jogo
criarJogoInicial palavra = JogoInternal {
    iPalavraSecreta = palavra,
    iLetrasChutadas = [],
    iTentativasRestantes = maxErros,
    iEstadoJogo = Jogando
}

-- | Retorna a palavra secreta de um jogo.
palavraSecretaJogo :: Jogo -> String
palavraSecretaJogo (JogoInternal ps _ _ _) = ps

-- | Retorna a lista de letras já chutadas.
letrasChutadasJogo :: Jogo -> [Char]
letrasChutadasJogo (JogoInternal _ lc _ _) = lc

-- | Retorna o número de tentativas restantes.
tentativasRestantesJogo :: Jogo -> Int
tentativasRestantesJogo (JogoInternal _ _ tr _) = tr

-- | Retorna o estado atual da partida.
estadoJogoJogo :: Jogo -> EstadoJogo
estadoJogoJogo (JogoInternal _ _ _ es) = es

-- | Função construtora para ser usada por módulos que calculam todos os novos valores
-- para um estado de Jogo. Módulos como LogicaJogo.hs usarão esta função.
construirJogoComNovosValores :: String -> [Char] -> Int -> EstadoJogo -> Jogo
construirJogoComNovosValores ps lc tr es = JogoInternal ps lc tr es

-- | Classe para objetos que podem ser exibidos como String formatada.
class Exibivel a where
    exibir :: a -> String  -- Converte um objeto em uma representação de string formatada

-- | Instância de Exibivel para o tipo Jogo.
instance Exibivel Jogo where
    exibir jogo =
        let
            -- Mostra a palavra com '_' para letras não adivinhadas
            palavraExibida = map (\c -> if c == ' ' || toUpper c `elem` map toUpper (letrasChutadasJogo jogo)
                                      then c
                                      else '_') (palavraSecretaJogo jogo)
            palavraFormatada = intersperse ' ' palavraExibida  -- Adiciona espaços entre as letras
            letrasFormatadas = intersperse ' ' (letrasChutadasJogo jogo)  -- Adiciona espaços entre as letras chutadas
            boneco = desenharBoneco (maxErros - tentativasRestantesJogo jogo)  -- Desenha o boneco da forca
        in
            boneco ++ "\n\n" ++
            "Palavra: " ++ palavraFormatada ++ "\n\n" ++
            "Letras já utilizadas: " ++ letrasFormatadas ++ "\n" ++
            "Tentativas restantes: " ++ show (tentativasRestantesJogo jogo)

-- | Desenha o boneco da forca de acordo com o número de erros.
desenharBoneco :: Int -> String
desenharBoneco erros = case erros of
    0 -> "  ____\n  |  |\n  |\n  |\n  |\n__|__"  -- Apenas a forca
    1 -> "  ____\n  |  |\n  |  O\n  |\n  |\n__|__"  -- Cabeça
    2 -> "  ____\n  |  |\n  |  O\n  |  |\n  |\n__|__"  -- Tronco
    3 -> "  ____\n  |  |\n  |  O\n  | /|\n  |\n__|__"  -- Braço esquerdo
    4 -> "  ____\n  |  |\n  |  O\n  | /|\\\n  |\n__|__"  -- Braço direito
    5 -> "  ____\n  |  |\n  |  O\n  | /|\\\n  | / \n__|__"  -- Perna esquerda
    _ -> "  ____\n  |  |\n  |  O\n  | /|\\\n  | / \\\n__|__"  -- Perna direita (completo)