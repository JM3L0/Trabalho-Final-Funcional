-- Tipos.hs
-- | Módulo que define os tipos de dados fundamentais do Jogo da Forca.
-- Este módulo implementa o padrão de design "Tipos Abstratos de Dados",
-- ocultando a implementação interna do tipo Jogo e fornecendo uma interface 
-- controlada para manipulá-lo.
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

import Data.List (intersperse)  -- Para inserir caracteres entre elementos de uma lista
import Data.Char (toUpper)      -- Para converter caracteres para maiúsculas

-- | O número máximo de erros permitidos antes de o jogador perder.
-- Este valor determina quantas tentativas erradas o jogador pode fazer antes de perder o jogo.
-- Também é usado para desenhar o boneco da forca progressivamente.
maxErros :: Int
maxErros = 6  -- 6 erros possíveis: cabeça, tronco, 2 braços e 2 pernas

-- | Enumeração dos possíveis estados do jogo.
-- Este tipo representa os três estados possíveis em que o jogo pode estar.
data EstadoJogo = 
    Jogando  -- ^ O jogo está em andamento, aguardando mais entradas do jogador
  | Ganhou   -- ^ O jogador adivinhou a palavra corretamente
  | Perdeu   -- ^ O jogador excedeu o número máximo de tentativas erradas
  deriving (Eq, Show)  -- Derivando automaticamente comparação de igualdade e conversão para string

-- | Tipo de dados principal que representa o estado do jogo.
-- Esta é uma implementação encapsulada, onde os detalhes internos não são exportados.
-- O construtor de dados `JogoInternal` e os nomes dos campos (prefixados com 'i')
-- não são exportados, tornando `Jogo` um tipo abstrato para módulos externos.
-- Esta técnica impede que outros módulos criem ou modifiquem instâncias de Jogo diretamente,
-- forçando-os a usar as funções de interface fornecidas.
data Jogo = JogoInternal {
    iPalavraSecreta :: String,      -- ^ A palavra que o jogador deve adivinhar
    iLetrasChutadas :: [Char],      -- ^ Letras que o jogador já tentou
    iTentativasRestantes :: Int,    -- ^ Número de erros ainda permitidos
    iEstadoJogo :: EstadoJogo       -- ^ Estado atual do jogo (Jogando/Ganhou/Perdeu)
} deriving (Show)  -- Permite converter um Jogo para string (útil para debugging)

-- --- Funções de Interface para o tipo Jogo (Exportadas) ---

-- | Cria um estado inicial para o jogo com uma palavra secreta.
-- Esta função é o único meio de criar um novo jogo a partir de uma palavra.
-- Inicializa o jogo com valores padrão: sem letras chutadas, todas as tentativas disponíveis,
-- e no estado "Jogando".
criarJogoInicial :: String -> Jogo
criarJogoInicial palavra = JogoInternal {
    iPalavraSecreta = palavra,  -- A palavra a ser adivinhada
    iLetrasChutadas = [],       -- Nenhuma letra foi chutada ainda
    iTentativasRestantes = maxErros,  -- Todas as tentativas estão disponíveis
    iEstadoJogo = Jogando       -- O jogo começa no estado "Jogando"
}

-- | Retorna a palavra secreta de um jogo.
-- Esta é uma função "getter" segura que permite acessar a palavra secreta
-- sem expor a estrutura interna do tipo Jogo.
palavraSecretaJogo :: Jogo -> String
palavraSecretaJogo (JogoInternal ps _ _ _) = ps

-- | Retorna a lista de letras já chutadas.
-- Esta é uma função "getter" segura para acessar as letras já tentadas pelo jogador.
letrasChutadasJogo :: Jogo -> [Char]
letrasChutadasJogo (JogoInternal _ lc _ _) = lc

-- | Retorna o número de tentativas restantes.
-- Esta é uma função "getter" segura para verificar quantas tentativas o jogador ainda tem.
tentativasRestantesJogo :: Jogo -> Int
tentativasRestantesJogo (JogoInternal _ _ tr _) = tr

-- | Retorna o estado atual da partida.
-- Esta é uma função "getter" segura para verificar se o jogo está em andamento,
-- se o jogador ganhou ou se perdeu.
estadoJogoJogo :: Jogo -> EstadoJogo
estadoJogoJogo (JogoInternal _ _ _ es) = es

-- | Função construtora para ser usada por módulos que calculam todos os novos valores
-- para um estado de Jogo. 
-- Esta função permite que módulos como LogicaJogo.hs criem novos estados de jogo
-- a partir de valores calculados, sem ter acesso direto ao construtor JogoInternal.
-- É essencialmente uma "função fábrica" controlada.
construirJogoComNovosValores :: String -> [Char] -> Int -> EstadoJogo -> Jogo
construirJogoComNovosValores ps lc tr es = JogoInternal ps lc tr es

-- | Classe para objetos que podem ser exibidos como String formatada.
-- Esta classe define uma interface para exibir objetos de forma amigável ao usuário.
-- É semelhante à classe Show padrão, mas com formatação específica para este jogo.
class Exibivel a where
    -- | Converte um objeto em uma representação de string formatada para exibição.
    exibir :: a -> String

-- | Instância de Exibivel para o tipo Jogo.
-- Define como um jogo deve ser apresentado visualmente ao usuário.
-- Inclui o desenho da forca, a palavra com letras adivinhadas, letras já usadas
-- e o número de tentativas restantes.
instance Exibivel Jogo where
    exibir jogo =
        let
            -- Determina como mostrar cada letra da palavra secreta:
            -- '_' para letras não adivinhadas, a própria letra para as adivinhadas
            palavraExibida = map (\c -> 
                                  if c == ' ' || toUpper c `elem` map toUpper (letrasChutadasJogo jogo)
                                      then c  -- Mostra a letra se for espaço ou já foi adivinhada
                                      else '_'  -- Mostra '_' para letras não adivinhadas
                                 ) (palavraSecretaJogo jogo)
            
            palavraFormatada = intersperse ' ' palavraExibida  -- Adiciona espaços entre as letras para melhor visualização
            letrasFormatadas = intersperse ' ' (letrasChutadasJogo jogo)  -- Adiciona espaços entre as letras chutadas
            boneco = desenharBoneco (maxErros - tentativasRestantesJogo jogo)  -- Desenha o boneco da forca conforme os erros
        in
            -- Constrói a string completa com todas as informações do jogo
            boneco ++ "\n\n" ++
            "Palavra: " ++ palavraFormatada ++ "\n\n" ++
            "Letras já utilizadas: " ++ letrasFormatadas ++ "\n" ++
            "Tentativas restantes: " ++ show (tentativasRestantesJogo jogo)

-- | Desenha o boneco da forca de acordo com o número de erros.
-- Esta função retorna uma representação ASCII do estado atual da forca,
-- adicionando partes do corpo conforme o número de erros aumenta.
desenharBoneco :: Int -> String
desenharBoneco erros = case erros of
    0 -> "  ____\n  |  |\n  |\n  |\n  |\n__|__"            -- Apenas a forca, sem erros
    1 -> "  ____\n  |  |\n  |  O\n  |\n  |\n__|__"         -- Cabeça (1 erro)
    2 -> "  ____\n  |  |\n  |  O\n  |  |\n  |\n__|__"      -- Tronco (2 erros)
    3 -> "  ____\n  |  |\n  |  O\n  | /|\n  |\n__|__"      -- Braço esquerdo (3 erros)
    4 -> "  ____\n  |  |\n  |  O\n  | /|\\\n  |\n__|__"    -- Braço direito (4 erros)
    5 -> "  ____\n  |  |\n  |  O\n  | /|\\\n  | / \n__|__" -- Perna esquerda (5 erros)
    _ -> "  ____\n  |  |\n  |  O\n  | /|\\\n  | / \\\n__|__" -- Perna direita (6+ erros, completo)