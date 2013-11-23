-- | Faz cenas sobre o tabuleiro
module Tabuleiro where

import Leitor

-- | Representa a lateral de uma peça
data Side = Field | City | Both
    deriving (Eq, Show)
-- | Representa as quatro laterais de uma peça
type Sides = (Side, Side, Side, Side)

-- | Remove duplicados de uma lista
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\vistos x -> if x `elem` vistos then vistos else vistos ++ [x]) []

-- obtém o tile que está numa posição, ou Nothing caso não exista nenhum nessa posição
getTileAtLocation :: [Tile] -> Location -> Maybe Tile
getTileAtLocation [] _ = Nothing
getTileAtLocation (tile@(Tile {t_x=tx, t_y=ty}):t) loc@(x,y) = if tx==x && ty==y then Just tile
                                                               else getTileAtLocation t loc

-- obtém os valores máximos e mínimos para as coordenadas X e Y
getLimits :: Board -> Limits
getLimits (Board {b_terrain=tiles}) = Limits { l_Xmin = xmin
                                             , l_Xmax = xmax
                                             , l_Ymin = ymin
                                             , l_Ymax = ymax
                                             }
                                         where (xmin, xmax) = getTileLimit (t_x) tiles
                                               (ymin, ymax) = getTileLimit (t_y) tiles

-- obtém o valor máximo para uma das coordenadas X ou Y, especificadas como t_x ou t_y
-- t_x :: (Tile->Int)
-- t_y :: (Tile->Int)
getTileLimit :: (Tile->Int) -> [Tile] -> (Int,Int)
getTileLimit member l = (minimum res, maximum res)
    where res = map (member) l
--getTileLimit member (h:t) [] = getTileLimit member t [(member h)]
--getTileLimit member (h:t) acum = getTileLimit member t ((member h):acum)
--getTileLimit member [] acum = (minimum acum, maximum acum)

-- | Recebe um 'Tile' e uma orientação e retorna as suas laterais (N,S,W,E)
borderTypeTile :: Tile -> Sides
borderTypeTile (Tile {t_type=tipo, t_orientation=orientacao}) = borderType tipo orientacao

-- | Recebe um tipo de peça e uma orientação e retorna as suas laterais (N,S,W,E)
borderType :: TileType -> Char -> Sides
borderType 'B' _ = (Field, Field, Field, Field)
borderType 'C' _ = (City, City, City, City)
borderType 'E' o = case o of
                    'N' -> (City, Field, Field, Field)
                    'E' -> (Field, City, Field, Field)
                    'S' -> (Field, Field, City, Field)
                    'W' -> (Field, Field, Field, City)
borderType 'N' o = case o of
                    'N' -> (City, Field, Field, City)
                    'E' -> (City, City, Field, Field)
                    'S' -> (Field, City, City, Field)
                    'W' -> (Field, Field, City, City)

-- | Informa que tipo de tile pode estar na posição (x,y)
possibleBordersOfTileAt :: Int -> Int -> [Tile] -> Sides
possibleBordersOfTileAt x y tiles = (southBorder (borderTypeMaybeTile up), westBorder (borderTypeMaybeTile right), northBorder (borderTypeMaybeTile down), eastBorder (borderTypeMaybeTile left))
                                    where up = getTileAtLocation tiles (x,y+1)
                                          left = getTileAtLocation tiles (x-1,y)
                                          down = getTileAtLocation tiles (x,y-1)
                                          right = getTileAtLocation tiles (x+1,y)

-- | Dado um 'Tile' obtém as suas bordas
borderTypeMaybeTile :: Maybe Tile -> Sides
borderTypeMaybeTile Nothing = (Both,Both,Both,Both)
borderTypeMaybeTile (Just x) = borderTypeTile x

-- | Dado os lados, retornar o lado de cima
northBorder :: Sides -> Side
northBorder (x,_,_,_) = x

-- | Dado os lados, retornar o lado da direita
eastBorder :: Sides -> Side
eastBorder (_,x,_,_) = x

-- | Dado os lados, retornar o lado de baixo
southBorder :: Sides -> Side
southBorder (_,_,x,_) = x

-- | Dado os lados, retornar o lado da esquerda
westBorder :: Sides -> Side
westBorder (_,_,_,x) = x

-- | Desdobra 'Sides' com lados ambíguos ('Both') numa lista de 'Sides' que apenas são 'Field' ou 'City'
removeAmbiguousSides :: Sides -> [Sides]
removeAmbiguousSides (Both, b, c, d) = (City,b,c,d):(Field,b,c,d):( (removeAmbiguousSides (City,b,c,d)) ++ (removeAmbiguousSides (Field,b,c,d)))
removeAmbiguousSides (a, Both, c, d) = (a,City,c,d):(a,Field,c,d):( (removeAmbiguousSides (a,City,c,d)) ++ (removeAmbiguousSides (a,Field,c,d)))
removeAmbiguousSides (a, b, Both, d) = (a,b,City,d):(a,b,Field,d):( (removeAmbiguousSides (a,b,City,d)) ++ (removeAmbiguousSides (a,b,Field,d)))
removeAmbiguousSides (a, b, c, Both) = (a,b,c,City):(a,b,c,Field):( (removeAmbiguousSides (a,b,c,City)) ++ (removeAmbiguousSides (a,b,c,Field)))
removeAmbiguousSides notAmbiguous = [notAmbiguous]

-- | Obtém uma lista de 'Tile' que podem ser colocados numa posição a partir de uma lista de 'Sides'
getTilesFromMaybeTiles :: [Maybe Tile] -> [Tile]
getTilesFromMaybeTiles [] = []
getTilesFromMaybeTiles (Nothing:xs) = getTilesFromMaybeTiles xs
getTilesFromMaybeTiles ((Just x):xs) = x:(getTilesFromMaybeTiles xs)

-- | Obtém o 'Tile' que se pode colocar tendo em conta determinado 'Sides'
getTilesFromSides :: Int -> Int -> [Sides] -> [Maybe Tile]
getTilesFromSides x y sides = foldr (++) [] (map (getTilesFromSide x y) sides)

-- | Verifica qual o tile que é possível colocar numa posição sabendo as laterais das peças adjacentes
getTilesFromSide :: Int -> Int -> Sides -> [Maybe Tile]
getTilesFromSide x y sides =
    case sides of
        (Field, Field, Field, Field) -> [Just (Tile { t_type = 'B', t_x = x, t_y = y, t_orientation = o, t_meeple = Nothing}) | o <- ['N','E','S','W']]
        (City, City, City, City) -> [Just (Tile { t_type = 'C', t_x = x, t_y = y, t_orientation = o, t_meeple = Nothing}) | o <- ['N','E','S','W']]
        (City, Field, Field, Field) -> [Just (Tile { t_type = 'E', t_x = x, t_y = y, t_orientation = 'N', t_meeple = Nothing})]
        (Field, City, Field, Field) -> [Just (Tile { t_type = 'E', t_x = x, t_y = y, t_orientation = 'E', t_meeple = Nothing})]
        (Field, Field, City, Field) -> [Just (Tile { t_type = 'E', t_x = x, t_y = y, t_orientation = 'S', t_meeple = Nothing})]
        (Field, Field, Field, City) -> [Just (Tile { t_type = 'E', t_x = x, t_y = y, t_orientation = 'W', t_meeple = Nothing})]
        (City, Field, Field, City) -> [Just (Tile { t_type = 'N', t_x = x, t_y = y, t_orientation = 'N', t_meeple = Nothing})]
        (City, City, Field, Field) -> [Just (Tile { t_type = 'N', t_x = x, t_y = y, t_orientation = 'E', t_meeple = Nothing})]
        (Field, City, City, Field) -> [Just (Tile { t_type = 'N', t_x = x, t_y = y, t_orientation = 'S', t_meeple = Nothing})]
        (Field, Field, City, City) -> [Just (Tile { t_type = 'N', t_x = x, t_y = y, t_orientation = 'W', t_meeple = Nothing})]
        _ -> [Nothing]


-- | Dado uma localização e a lista de 'Tile' em jogo, informa que peças podem ser lá colocadas
possibleTilesAt :: [Tile] -> Location -> [Tile]
possibleTilesAt tiles (x,y) =
    case getTileAtLocation tiles (x,y) of
        Nothing -> case (possibleBordersOfTileAt x y tiles) of
                     (Both, Both, Both, Both) -> [] -- não há nenhum tile adjacente
                     sides -> getTilesFromMaybeTiles (getTilesFromSides x y (removeDuplicates (removeAmbiguousSides sides)))
        (Just _) -> []

-- | A partir do 'Board' descobre todas as peças que podem ser colocadas, onde e em que posição (não verifica meeples)
possibleNextTiles :: Board -> [Tile]
possibleNextTiles b = concat (map (possibleTilesAt tiles) positions)
    where limits = (getLimits b)
          tiles = b_terrain b
          (xmin,xmax,ymin,ymax) = ((l_Xmin limits)-1, (l_Xmax limits)+1, (l_Ymin limits)-1, (l_Ymax limits)+1)
          positions = concat [[(x,y) | x <- [xmin..xmax]] | y <- [ymax, (ymax-1)..ymin] ]