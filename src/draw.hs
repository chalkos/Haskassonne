import Data.Char
import Text.XML.Light
import Text.XML.Light.Types

-- definição de tipos
type TileType = Char
type Player = Int

data Score = Score {
    s_player :: Player,
    s_score :: Int
} deriving (Show)

data Next = Next {
    n_player :: Player,
    n_tile :: TileType
} deriving (Show)

data Meeple = Meeple {
    m_player :: Player,
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
    b_players :: Int,
    b_terrain :: [Tile],
    b_scores :: [Score],
    b_next :: Next
} deriving (Show)

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          putStrLn $ (processa elem)

processa :: Element -> String
processa e = show (processaBoard e)

-- auxiliar
getElemsFromContent :: [Content] -> [Element]
getElemsFromContent [] = []
getElemsFromContent ((Elem e):t) = e:(getElemsFromContent t)
getElemsFromContent (_:t) = getElemsFromContent t

processaBoard :: Element -> Board
processaBoard (Element {elName=(QName {qName="board"}), elAttribs=((Attr {attrKey=(QName {qName="players"}), attrVal=n}):[]), elContent=children} ) = 
    Board { b_players = read n
          , b_terrain = processaTerrain (getElemsFromContent children)
          , b_scores  = processaScores (getElemsFromContent children)
          , b_next    = processaNext (getElemsFromContent children)
          }

processaTerrain :: [Element] -> [Tile]
processaTerrain _ = []

processaScores :: [Element] -> [Score]
processaScores _ = []

-- se não encontrar a tag next dá erro, como é suposto
processaNext :: [Element] -> Next
processaNext ((Element {elName=(QName {qName="next"}), elAttribs=attr} ):_) =
        Next {n_player=(processaNextAttr_NPlayer attr), n_tile=(processaNextAttr_NTile attr)}
processaNext (_:t) = processaNext t

processaNextAttr_NPlayer :: [Attr] -> Int
processaNextAttr_NPlayer ((Attr {attrKey=(QName {qName="player"}), attrVal=n}):_) = read n
processaNextAttr_NPlayer (_:t) = processaNextAttr_NPlayer t

processaNextAttr_NTile :: [Attr] -> Char
processaNextAttr_NTile ((Attr {attrKey=(QName {qName="tile"}), attrVal=(c:_)}):_) = toUpper c
processaNextAttr_NTile (_:t) = processaNextAttr_NTile t


--Element  
--elName :: QName
--elAttribs :: [Attr]
--elContent :: [Content]
--elLine :: Maybe Line
