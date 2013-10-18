import Data.Char
import Text.XML.Light
import Text.XML.Light.Types
import Text.Show.Pretty

-- definição de tipos
type TileType = Char
type Player = Int
type Map = [[Maybe Tile]]
type Location = (Int, Int)

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

data Limits = Limits {
    l_Xmin :: Int,
    l_Xmax :: Int,
    l_Ymin :: Int,
    l_Ymax :: Int
} deriving (Show)

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          putStrLn $ (processa elem)

processa :: Element -> String
--processa e = ppShow (processaBoard e)
--processa e = ppShow (getLimits (processaBoard e))
processa e = ppShow (buildMap (b_terrain board) (getLimits board))
             where board = processaBoard e

--------------------
-- fazer o artASCII
--------------------
getLimits :: Board -> Limits
getLimits (Board {b_terrain=tiles}) = Limits { l_Xmin = xmin
                                             , l_Xmax = xmax
                                             , l_Ymin = ymin
                                             , l_Ymax = ymax
                                             }
                                         where (xmin, xmax) = getTileLimit (t_x) tiles []
                                               (ymin, ymax) = getTileLimit (t_y) tiles []

getTileLimit :: (Tile->Int) -> [Tile] -> [Int] -> (Int,Int)
getTileLimit member (h:t) [] = getTileLimit member t [(member h)]
getTileLimit member (h:t) acum = getTileLimit member t ((member h):acum)
getTileLimit member [] acum = (minimum acum, maximum acum)

buildMap :: [Tile] -> Limits -> Map
buildMap tiles dim = if ymax /= ymin then [ [(getTileAtLocation tiles (x,y)) | x <- [xmin..xmax]] | y <- [ymax, (ymax-1)..ymin] ]
                     else [ [(getTileAtLocation tiles (x,y)) | x <- [xmin..xmax]] | y <- [ymax] ]
                          where xmin = l_Xmin dim
                                xmax = l_Xmax dim
                                ymin = l_Ymin dim
                                ymax = l_Ymax dim

-- constroi uma matriz de Tile (ou seja, um Map) organizado de acordo com as localizações dos tiles
{-buildMap :: [Tile] -> Limits -> Location -> Map
buildMap tiles dim@(Limits {l_Ymin=ymin, l_Ymax=ymax}) (incX, incY) = 
    if y <= ymax then [buildMapX tiles dim (incX, incY)]:(buildMap tiles dim (incX, incY))
    else 
    where x = xmin+incX
          y = ymin+incY

buildMapX :: [Tile] -> Limits -> Location -> [Maybe Tile]
buildMapX tiles dim@(Limits {l_Xmin=xmin, l_Xmax=xmax, l_Ymin=ymin}) (incX, incY) =
    if x <= xmax then (getTileAtLocation tiles (x,y)):(buildMapX tiles dim (incX+1, incY))
    else []
    where x = xmin+incX
          y = ymin+incY-}

getTileAtLocation :: [Tile] -> Location -> Maybe Tile
getTileAtLocation [] _ = Nothing
getTileAtLocation (tile@(Tile {t_x=tx, t_y=ty}):t) loc@(x,y) = if tx==x && ty==y then Just tile
                                                               else getTileAtLocation t loc

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
processaBoard (Element {elName=(QName {qName="board"}), elAttribs=((Attr {attrKey=(QName {qName="players"}), attrVal=n}):[]), elContent=children} ) = 
    Board { b_players = read n
          , b_terrain = processaTerrain (getElemsFromContent children)
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
processaScores :: [Element] -> [Score]
processaScores ((Element {elName=(QName {qName="scores"}), elContent=children} ):_) = processaScore (getElemsFromContent children)
processaScores (_:t) = processaScores t

-- encontra as tags score
processaScore :: [Element] -> [Score]
processaScore [] = []
processaScore ((Element {elName=(QName {qName="score"}), elAttribs=attr} ):t) =
        (Score {s_player=(getAttrValueInt "player" attr), s_score=(getAttrValueInt "score" attr)}):(processaScore t)

-- encontra a tag next
processaNext :: [Element] -> Next
processaNext ((Element {elName=(QName {qName="next"}), elAttribs=attr} ):_) =
        Next {n_player=(getAttrValueInt "player" attr), n_tile=(getAttrValueChar "tile" attr)}
processaNext (_:t) = processaNext t
