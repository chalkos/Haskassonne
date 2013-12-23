-- | Lê as informações do XML para as estruturas do Haskassone.
module Leitor where

import Data.Char
import Text.XML.Light
import Text.XML.Light.Types
--import Text.Show.Pretty
import FakePrettyShow

-- | Um jogador. O jogador é identificado por um valor inteiro e tem uma pontuação.
data Player = Player {
    s_player :: Int,
    s_score :: Int
} deriving (Show)

-- | Informações sobre a pŕoxima jogada, nomeadamente o jogador que se segue e a peça que este deve colocar.
data Next = Next {
    n_player :: Int,
    n_tile :: TileType
} deriving (Show)

-- | Um Meeple, com informação sobre o seu dono e tipo. O tipo do Meeple (F - Farmer, K - Knight ou M - Monk) define o sítio onde este está colocado num 'Tile'.
data Meeple = Meeple {
    m_player :: Int,
    m_type :: Char
} deriving (Eq, Show)

-- | Uma peça de Terreno.
data Tile = Tile {
    -- | O tipo da peça.
    t_type :: TileType,
    -- | Posição horizontal da peça.
    t_x :: Int,
    -- | Posição vertical da peça.
    t_y :: Int,
    -- | Orientação da peça (N,S,W,E).
    t_orientation :: Char,
    -- | Um Meeple que exista na peça.
    t_meeple :: Maybe Meeple
} deriving (Eq, Show)

-- | O terreno.
data Board = Board {
    -- | As peças do terreno.
    b_terrain :: [Tile],
    -- | Os jogadores.
    b_scores :: [Player],
    -- | A próxima Jogada.
    b_next :: Next
} deriving (Show)

-- | Um caracter (B,C,E ou N) que identifica o tipo da peça.
type TileType = Char
-- | Uma matriz de peças para representar um mapa.
type Map = [[Maybe Tile]]
-- | Um ponto de coordenadas (x,y).
type Location = (Int, Int)
-- | Os limites do mapa.
data Limits = Limits {
    l_Xmin :: Int,
    l_Xmax :: Int,
    l_Ymin :: Int,
    l_Ymax :: Int
} deriving (Show)

----------------------------------
-- passar de xml para a estrutura
----------------------------------

-- | Obtém apenas os Element de um conjunto de Content.
getElemsFromContent :: [Content] -> [Element]
getElemsFromContent [] = []
getElemsFromContent ((Elem e):t) = e:(getElemsFromContent t)
getElemsFromContent (_:t) = getElemsFromContent t

-- | Encontra o valor inteiro para determinada chave num conjunto de atributos.
getAttrValueInt :: String -> [Attr] -> Int
getAttrValueInt key ((Attr {attrKey=(QName {qName=attrK}), attrVal=attrV}):t) = if attrK == key then (read attrV)
                                                                                else getAttrValueInt key t

-- | Encontra o valor char para determinada chave num conjunto de atributos.
getAttrValueChar :: String -> [Attr] -> Char
getAttrValueChar key ((Attr {attrKey=(QName {qName=attrK}), attrVal=attrV}):t) = if attrK == key then (toUpper (head attrV))
                                                                                else getAttrValueChar key t

-- | Encontra a tag \<board> e processa os seus descendentes.
processaBoard :: Element -> Board
processaBoard (Element {elName=(QName {qName="board"}), elContent=children} ) = 
    Board { b_terrain = processaTerrain (getElemsFromContent children)
          , b_scores  = processaScores (getElemsFromContent children)
          , b_next    = processaNext (getElemsFromContent children)
          }

-- | Encontra a tag \<terrain> e processa os seus descendentes.
processaTerrain :: [Element] -> [Tile]
processaTerrain ((Element {elName=(QName {qName="terrain"}), elContent=children} ):_) = processaTile (getElemsFromContent children)
processaTerrain (_:t) = processaTerrain t

-- | Encontra a tag \<title> e processa os seus atributos e descendentes.
processaTile :: [Element] -> [Tile]
processaTile [] = []
processaTile ((Element {elName=(QName {qName="tile"}), elAttribs=attr, elContent=children} ):t) =
        (Tile { t_type = getAttrValueChar "type" attr
              , t_x = getAttrValueInt "x" attr
              , t_y = getAttrValueInt "y" attr
              , t_orientation = getAttrValueChar "orientation" attr
              , t_meeple = processaMeeple (getElemsFromContent children)
              }):(processaTile t)

-- | Tenta encontrar uma tag \<follower> e processa os seus atributos. Caso encontre constroi um 'Meeple'.
processaMeeple :: [Element] -> Maybe Meeple
processaMeeple [] = Nothing
processaMeeple ((Element {elName=(QName {qName="follower"}), elAttribs=attr} ):_) =
        Just (Meeple { m_player = getAttrValueInt "player" attr
                     , m_type = getAttrValueChar "type" attr
                     })

-- | Encontra a tag \<scores> e processa os seus descendentes.
processaScores :: [Element] -> [Player]
processaScores ((Element {elName=(QName {qName="scores"}), elContent=children} ):_) = processaScore (getElemsFromContent children)
processaScores (_:t) = processaScores t

-- | Encontra as tags \<score>, processa os seus atributos e constroi uma lista de 'Player'.
processaScore :: [Element] -> [Player]
processaScore [] = []
processaScore ((Element {elName=(QName {qName="score"}), elAttribs=attr} ):t) =
        (Player {s_player=(getAttrValueInt "player" attr), s_score=(getAttrValueInt "score" attr)}):(processaScore t)

-- | Encontra a tag \<next> e processa os seus atributos num 'Next'.
processaNext :: [Element] -> Next
processaNext [] = Next 0 '-' --estes valores são inválidos e permitem apenas que o draw seja executado depois do jogo terminar
processaNext ((Element {elName=(QName {qName="next"}), elAttribs=attr} ):_) =
        Next {n_player=(getAttrValueInt "player" attr), n_tile=(getAttrValueChar "tile" attr)}
processaNext (_:t) = processaNext t

