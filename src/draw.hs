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

-- Obtém apenas os Element de um conjunto de Content
getElemsFromContent :: [Content] -> [Element]
getElemsFromContent [] = []
getElemsFromContent ((Elem e):t) = e:(getElemsFromContent t)
getElemsFromContent (_:t) = getElemsFromContent t

-- encontra o valor inteiro para determinada chave num conjunto de atributos
getAttrValueInt :: String -> [Attr] -> Int
--getAttrValueInt [] = ????
getAttrValueInt key ((Attr {attrKey=(QName {qName=attrK}), attrVal=attrV}):t) = if attrK == key then (read attrV)
                                                                                else getAttrValueInt key t

-- encontra o valor char para determinada chave num conjunto de atributos
getAttrValueChar :: String -> [Attr] -> Char
--getAttrValueChar [] = ????
getAttrValueChar key ((Attr {attrKey=(QName {qName=attrK}), attrVal=attrV}):t) = if attrK == key then (toUpper (head attrV))
                                                                                else getAttrValueChar key t

-- board
processaBoard :: Element -> Board
processaBoard (Element {elName=(QName {qName="board"}), elAttribs=((Attr {attrKey=(QName {qName="players"}), attrVal=n}):[]), elContent=children} ) = 
    Board { b_players = read n
          , b_terrain = processaTerrain (getElemsFromContent children)
          , b_scores  = processaScores (getElemsFromContent children)
          , b_next    = processaNext (getElemsFromContent children)
          }

-- terrain
processaTerrain :: [Element] -> [Tile]
processaTerrain _ = []

-- encontra a tag scores
processaScores :: [Element] -> [Score]
processaScores ((Element {elName=(QName {qName="scores"}), elContent=children} ):_) = processaScore (getElemsFromContent children)
processaScores (_:t) = processaScores t

-- encontra as tags score
processaScore :: [Element] -> [Score]
processaScore [] = []
processaScore ((Element {elName=(QName {qName="score"}), elAttribs=attr} ):t) =
        (Score {s_player=(getAttrValueInt "player" attr), s_score=(getAttrValueInt "score" attr)}):(processaScore t)
-- processaScore (_:t) = processaScore t -- não deve ser preciso a não ser que o xml esteja mal formatado

-- encontra a tag next
-- se não encontrar a tag next dá erro, como é suposto
processaNext :: [Element] -> Next
processaNext ((Element {elName=(QName {qName="next"}), elAttribs=attr} ):_) =
        Next {n_player=(getAttrValueInt "player" attr), n_tile=(getAttrValueChar "tile" attr)}
processaNext (_:t) = processaNext t


--Element  
--elName :: QName
--elAttribs :: [Attr]
--elContent :: [Content]
--elLine :: Maybe Line
