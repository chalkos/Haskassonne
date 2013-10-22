import Data.Char
import Text.XML.Light
import Text.XML.Light.Types
import Text.Show.Pretty
import Data.List.Utils

-- definição de tipos
type TileType = Char
type Player = Int
type Map = [[Maybe Tile]]
type Location = (Int, Int)
type Art = [String]

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
{-processa e = ppShow (buildMap (b_terrain board) (getLimits board))
             where board = processaBoard e-}
--processa e = ppShow (rotateArt artTest 'N')
processa e = drawMap (buildMap (b_terrain board) (getLimits board))
             where board = processaBoard e

--------------------
-- fazer o artASCII
--------------------
-- as peças do jogo
artB = ["..F..", "..O..", ".OMO.", "..O..", "....."] :: Art
artC = ["*****", "*****", "**K**", "*****", "*****"] :: Art
artE = ["*****", ".*K*.", "..*..", ".....", "..F.."] :: Art
artN = ["*****", "*K**.", "***..", "**.F.", "*...."] :: Art
artVoid = ["     ","     ","     ","     ","     "] :: Art
artTest = ["ABCDE", "FGHIJ", "KLMNO", "PQRST", "UVWXY"] :: Art

-- obtém os valores máximos e mínimos para as coordenadas X e Y
getLimits :: Board -> Limits
getLimits (Board {b_terrain=tiles}) = Limits { l_Xmin = xmin
                                             , l_Xmax = xmax
                                             , l_Ymin = ymin
                                             , l_Ymax = ymax
                                             }
                                         where (xmin, xmax) = getTileLimit (t_x) tiles []
                                               (ymin, ymax) = getTileLimit (t_y) tiles []

-- obtém o valor máximo para uma das coordenadas X ou Y, especificadas como t_x ou t_y
-- t_x :: (Tile->Int)
-- t_y :: (Tile->Int)
getTileLimit :: (Tile->Int) -> [Tile] -> [Int] -> (Int,Int)
getTileLimit member (h:t) [] = getTileLimit member t [(member h)]
getTileLimit member (h:t) acum = getTileLimit member t ((member h):acum)
getTileLimit member [] acum = (minimum acum, maximum acum)

-- organiza uma matriz de Maybe Tile
buildMap :: [Tile] -> Limits -> Map
buildMap tiles dim = if ymax /= ymin then [ [(getTileAtLocation tiles (x,y)) | x <- [xmin..xmax]] | y <- [ymax, (ymax-1)..ymin] ]
                     else [ [(getTileAtLocation tiles (x,y)) | x <- [xmin..xmax]] | y <- [ymax] ]
                          where xmin = l_Xmin dim
                                xmax = l_Xmax dim
                                ymin = l_Ymin dim
                                ymax = l_Ymax dim

-- Converte os tiles do mapa para Arts e depois transforma-os numa string
drawMap :: Map -> String
drawMap mapa = artTiles2String (map (\y-> (map (\x->tile2Art x) y)) mapa)

-- constroi uma string com a matriz de Art
artTiles2String :: [[Art]] -> String
artTiles2String [] = ""
artTiles2String (h1@(([]):t2):t1) = artTiles2String t1
artTiles2String (h1:t1) = (concat (getAllHeads h1)) ++ "\n" ++ (artTiles2String ((getAllTails h1):t1))
                        where
                            getAllHeads :: [[a]] -> [a]
                            getAllHeads [] = [] 
                            getAllHeads ((a:b):c) = a:(getAllHeads c)
                            getAllTails :: [[a]] -> [[a]]
                            getAllTails [] = []
                            getAllTails ((a:b):c) = [b] ++ (getAllTails c)

-- dado um tile, retorna o artASCII correspondente
tile2Art :: Maybe Tile -> Art
tile2Art Nothing = artVoid
tile2Art (Just (Tile {t_type=nome, t_orientation=rot, t_meeple=meeple})) = rotateArt (drawMeeple art meeple) rot
    where
        art | nome == 'B' = artB
            | nome == 'C' = artC
            | nome == 'E' = artE
            | nome == 'N' = artN

-- modifica o desenho conforme o Meeple que lá estiver colocado
drawMeeple :: Art -> Maybe Meeple -> Art
drawMeeple art Nothing = map (\x -> (replace "F" "." (replace "K" "*" (replace "M" "O" x)))) art
drawMeeple art (Just (Meeple {m_player=player, m_type=name})) = drawMeeple (map (\x -> (replace (name:[]) (show player) x)) art) Nothing

-- dada uma posição orientada a norte e uma orientação, retorna a posição rodada
-- não é utilizada, por enquanto
coord :: Location -> Char -> Location
coord (x,y) 'N' = ( x , y )
coord (x,y) 'E' = (4-y, x )
coord (x,y) 'S' = (4-x,4-y)
coord (x,y) 'W' = ( y ,4-x)

-- data uma posição e uma orientação, retorna a posição correspondente numa peça orientada a norte
coordInv :: Location -> Char -> Location
coordInv (x,y) 'N' = ( x , y )
coordInv (x,y) 'E' = ( y ,4-x)
coordInv (x,y) 'S' = (4-x,4-y)
coordInv (x,y) 'W' = (4-y, x )

-- faz a rotação da peça
rotateArt :: Art -> Char -> Art
rotateArt art rot = [ [ (getMatrixValue art (coordInv (x,y) rot)) | x <- [0..4] ] | y <- [0..4] ]

-- obtém o valor numa posição da matriz (zero-based)
getMatrixValue :: [[a]] -> Location -> a
getMatrixValue l (x,y) = ((l !! y) !! x)

-- obtém o tile que está numa posição, ou Nothing caso não exista nenhum nessa posição
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
