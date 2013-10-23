module Comum where

import Data.Char
import Text.XML.Light
import Text.XML.Light.Types
import Text.Show.Pretty

data Player = Player {
    s_player :: Int,
    s_score :: Int
} deriving (Show)

data Next = Next {
    n_player :: Int,
    n_tile :: TileType
} deriving (Show)

data Meeple = Meeple {
    m_player :: Int,
    m_type :: Char
} deriving (Show)

data Tile = Tile {
    t_type :: TileType,
    t_x :: Int,
    t_y :: Int,
    t_orientation :: Char,
    t_meeple :: Maybe Meeple
} deriving (Show)

data Board = Board {
    b_terrain :: [Tile],
    b_scores :: [Player],
    b_next :: Next
} deriving (Show)

-- definição de tipos
type TileType = Char
type Map = [[Maybe Tile]]
type Location = (Int, Int)
type Art = [String]

data Limits = Limits {
    l_Xmin :: Int,
    l_Xmax :: Int,
    l_Ymin :: Int,
    l_Ymax :: Int
} deriving (Show)

----------------------------------
-- passar de xml para a estrutura
----------------------------------

-- Obtém apenas os Element de um conjunto de Content
getElemsFromContent :: [Content] -> [Element]
getElemsFromContent [] = []
getElemsFromContent ((Elem e):t) = e:(getElemsFromContent t)
getElemsFromContent (_:t) = getElemsFromContent t

-- encontra o valor inteiro para determinada chave num conjunto de atributos
getAttrValueInt :: String -> [Attr] -> Int
getAttrValueInt key ((Attr {attrKey=(QName {qName=attrK}), attrVal=attrV}):t) = if attrK == key then (read attrV)
                                                                                else getAttrValueInt key t

-- encontra o valor char para determinada chave num conjunto de atributos
getAttrValueChar :: String -> [Attr] -> Char
getAttrValueChar key ((Attr {attrKey=(QName {qName=attrK}), attrVal=attrV}):t) = if attrK == key then (toUpper (head attrV))
                                                                                else getAttrValueChar key t

-- board
processaBoard :: Element -> Board
processaBoard (Element {elName=(QName {qName="board"}), elContent=children} ) = 
    Board { b_terrain = processaTerrain (getElemsFromContent children)
          , b_scores  = processaScores (getElemsFromContent children)
          , b_next    = processaNext (getElemsFromContent children)
          }

-- encontra a tag terrain
processaTerrain :: [Element] -> [Tile]
processaTerrain ((Element {elName=(QName {qName="terrain"}), elContent=children} ):_) = processaTile (getElemsFromContent children)
processaTerrain (_:t) = processaTerrain t

-- encontra as tags tile
processaTile :: [Element] -> [Tile]
processaTile [] = []
processaTile ((Element {elName=(QName {qName="tile"}), elAttribs=attr, elContent=children} ):t) =
        (Tile { t_type = getAttrValueChar "type" attr
              , t_x = getAttrValueInt "x" attr
              , t_y = getAttrValueInt "y" attr
              , t_orientation = getAttrValueChar "orientation" attr
              , t_meeple = processaMeeple (getElemsFromContent children)
              }):(processaTile t)

-- encontra a tag follower
processaMeeple :: [Element] -> Maybe Meeple
processaMeeple [] = Nothing
processaMeeple ((Element {elName=(QName {qName="follower"}), elAttribs=attr} ):_) =
        Just (Meeple { m_player = getAttrValueInt "player" attr
                     , m_type = getAttrValueChar "type" attr
                     })

-- encontra a tag scores
processaScores :: [Element] -> [Player]
processaScores ((Element {elName=(QName {qName="scores"}), elContent=children} ):_) = processaScore (getElemsFromContent children)
processaScores (_:t) = processaScores t

-- encontra as tags score
processaScore :: [Element] -> [Player]
processaScore [] = []
processaScore ((Element {elName=(QName {qName="score"}), elAttribs=attr} ):t) =
        (Player {s_player=(getAttrValueInt "player" attr), s_score=(getAttrValueInt "score" attr)}):(processaScore t)

-- encontra a tag next
processaNext :: [Element] -> Next
processaNext ((Element {elName=(QName {qName="next"}), elAttribs=attr} ):_) =
        Next {n_player=(getAttrValueInt "player" attr), n_tile=(getAttrValueChar "tile" attr)}
processaNext (_:t) = processaNext t
