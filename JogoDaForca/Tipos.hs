-- Tipos.hs
module Tipos (
    -- * Tipos Principais
    Jogo,               -- Exporta apenas o TIPO Jogo, não seus construtores/campos internos
    EstadoJogo(..),
    -- * Classe Exibivel
    Exibivel(..),       -- Exporta a classe Exibivel e seu método
    -- * Constantes
    maxErros,
    -- * Interface para o tipo Jogo
    criarJogoInicial,
    palavraSecretaJogo,
    letrasChutadasJogo,
    tentativasRestantesJogo,
    estadoJogoJogo,
    -- Função para módulos externos construírem um novo estado de Jogo
    construirJogoComNovosValores
) where

import Data.List (intersperse)
import Data.Char (toUpper)

-- | O número máximo de erros permitidos antes de o jogador perder.
maxErros :: Int
maxErros = 6

-- | Enumeração dos possíveis estados do jogo.
data EstadoJogo = Jogando
                | Ganhou
                | Perdeu
                deriving (Eq, Show)

-- | Tipo de dados principal que representa o estado do jogo.
-- O construtor de dados `JogoInternal` e os nomes dos campos (prefixados com 'i')
-- não são exportados, tornando `Jogo` um tipo abstrato para módulos externos.
data Jogo = JogoInternal {
    iPalavraSecreta :: String,
    iLetrasChutadas :: [Char],
    iTentativasRestantes :: Int,
    iEstadoJogo :: EstadoJogo
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
    exibir :: a -> String

-- | Instância de Exibivel para o tipo Jogo.
instance Exibivel Jogo where
    exibir jogo =
        let
            palavraExibida = map (\c -> if c == ' ' || toUpper c `elem` map toUpper (letrasChutadasJogo jogo)
                                      then c
                                      else '_') (palavraSecretaJogo jogo)
            palavraFormatada = intersperse ' ' palavraExibida
            letrasFormatadas = intersperse ' ' (letrasChutadasJogo jogo)
            boneco = desenharBoneco (maxErros - tentativasRestantesJogo jogo)
        in
            boneco ++ "\n\n" ++
            "Palavra: " ++ palavraFormatada ++ "\n\n" ++
            "Letras já utilizadas: " ++ letrasFormatadas ++ "\n" ++
            "Tentativas restantes: " ++ show (tentativasRestantesJogo jogo)

-- | Desenha o boneco da forca de acordo com o número de erros.
desenharBoneco :: Int -> String
desenharBoneco erros = case erros of
    0 -> "  ____\n  |  |\n  |\n  |\n  |\n__|__"
    1 -> "  ____\n  |  |\n  |  O\n  |\n  |\n__|__"
    2 -> "  ____\n  |  |\n  |  O\n  |  |\n  |\n__|__"
    3 -> "  ____\n  |  |\n  |  O\n  | /|\n  |\n__|__"
    4 -> "  ____\n  |  |\n  |  O\n  | /|\\\n  |\n__|__"
    5 -> "  ____\n  |  |\n  |  O\n  | /|\\\n  | / \n__|__"
    _ -> "  ____\n  |  |\n  |  O\n  | /|\\\n  | / \\\n__|__"