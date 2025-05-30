-- | Definições de tipos de dados para o Jogo da Forca.
-- Este módulo centraliza os tipos principais usados pelo jogo.
module Tipos (
    -- * Tipos Principais
    Jogo(..),           -- ^ O tipo de dados principal que representa o estado do jogo.
    EstadoJogo(..),     -- ^ Enumeração dos possíveis estados do jogo.
    -- * Classe Exibivel
    Exibivel(..),       -- ^ Classe para objetos que podem ser exibidos como String.
    -- * Constantes
    maxErros            -- ^ Número máximo de erros permitidos.
) where

import Data.List (intersperse)
import Data.Char (toUpper)

-- | O número máximo de erros permitidos antes de o jogador perder.
maxErros :: Int
maxErros = 6

-- | Enumeração dos possíveis estados do jogo.
data EstadoJogo = Jogando  -- ^ O jogo está em andamento.
                | Ganhou   -- ^ O jogador ganhou o jogo.
                | Perdeu   -- ^ O jogador perdeu o jogo.
                deriving (Eq, Show)

-- | Tipo de dados principal que representa o estado do jogo.
data Jogo = Jogo {
    palavraSecreta :: String,      -- ^ A palavra que o jogador está tentando adivinhar.
    letrasChutadas :: [Char],      -- ^ Letras que já foram tentadas pelo jogador.
    tentativasRestantes :: Int,    -- ^ Número de tentativas incorretas restantes.
    estadoJogo :: EstadoJogo       -- ^ Estado atual do jogo (Jogando, Ganhou, Perdeu).
} deriving (Show)

-- | Classe para objetos que podem ser exibidos como String formatada.
class Exibivel a where
    exibir :: a -> String

-- | Instância de Exibivel para o tipo Jogo.
instance Exibivel Jogo where
    exibir jogo =
        let
            -- Formata a palavra secreta substituindo letras não adivinhadas por '_'
            palavraExibida = map (\c -> if c == ' ' || toUpper c `elem` map toUpper (letrasChutadas jogo)
                                      then c
                                      else '_') (palavraSecreta jogo)
            -- Junta os caracteres com espaços para melhor visualização
            palavraFormatada = intersperse ' ' palavraExibida

            -- Formata as letras chutadas
            letrasFormatadas = intersperse ' ' (letrasChutadas jogo)

            -- Formata o boneco da forca
            boneco = desenharBoneco (maxErros - tentativasRestantes jogo)
        in
            -- Retorna a representação completa do jogo
            boneco ++ "\n\n" ++
            "Palavra: " ++ palavraFormatada ++ "\n\n" ++
            "Letras já utilizadas: " ++ letrasFormatadas ++ "\n" ++
            "Tentativas restantes: " ++ show (tentativasRestantes jogo)

-- | Desenha o boneco da forca de acordo com o número de erros.
desenharBoneco :: Int -> String
desenharBoneco erros = case erros of
    0 -> "  ____\n  |  |\n  |\n  |\n  |\n__|__"
    1 -> "  ____\n  |  |\n  |  O\n  |\n  |\n__|__"
    2 -> "  ____\n  |  |\n  |  O\n  |  |\n  |\n__|__"
    3 -> "  ____\n  |  |\n  |  O\n  | /|\n  |\n__|__"
    4 -> "  ____\n  |  |\n  |  O\n  | /|\\\n  |\n__|__"
    5 -> "  ____\n  |  |\n  |  O\n  | /|\\\n  | /\n__|__"
    _ -> "  ____\n  |  |\n  |  O\n  | /|\\\n  | / \\\n__|__"