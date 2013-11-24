-- | Faz cenas sobre o tabuleiro
module Tabuleiro where

import Leitor
import Data.List
import Data.Maybe
import Data.Char


-- | Números de peças que podem ser jogadas
nPecasB = 6 :: Int
nPecasC = 2 :: Int
nPecasE = 18 :: Int
nPecasN = 10 :: Int
nPecasT = nPecasB + nPecasC + nPecasE + nPecasN :: Int

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
getSidesFromTile :: Tile -> Sides
getSidesFromTile (Tile {t_type=tipo, t_orientation=orientacao}) = getSidesFromTypeAndOrientation tipo orientacao

-- | Recebe um tipo de peça e uma orientação e retorna as suas laterais (N,S,W,E)
getSidesFromTypeAndOrientation :: TileType -> Char -> Sides
getSidesFromTypeAndOrientation 'B' _ = (Field, Field, Field, Field)
getSidesFromTypeAndOrientation 'C' _ = (City, City, City, City)
getSidesFromTypeAndOrientation 'E' o = case o of
                                            'N' -> (City, Field, Field, Field)
                                            'E' -> (Field, City, Field, Field)
                                            'S' -> (Field, Field, City, Field)
                                            'W' -> (Field, Field, Field, City)
getSidesFromTypeAndOrientation 'N' o = case o of
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
borderTypeMaybeTile (Just x) = getSidesFromTile x

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

-- | Obter o tipo do próximo tile
getNextTileType :: Board -> TileType
getNextTileType (Board {b_next=proximaJogada}) = n_tile proximaJogada

-- | Obter o próximo jogador
getNextPlayer :: Board -> Player
getNextPlayer (Board {b_next=proximaJogada, b_scores=players}) = 
  case find ((proxJogador==).s_player) players of
    Just match -> match
  where proxJogador = n_player proximaJogada

-- | Verifica se o próximo jogador tem meeples por jogar
nextPlayerHasMeeples :: Board -> Bool
nextPlayerHasMeeples board = (meeplesPlacedByNextPlayer board) <= 7

-- | Verifica quantos meeples o próximo jogador tem em jogo
meeplesPlacedByNextPlayer :: Board -> Int
meeplesPlacedByNextPlayer board = length (filter ((nPlayer==).m_player) (map (fromJust) (filter (isJust) (map (t_meeple) (b_terrain board)))))
                                  where nPlayer = n_player (b_next board)

-- verificar a peça seguinte e filtrar as possiveis
-- verificar se o jogador pode usar meeples
-- | Obter os 'Tile's que podem ser jogados, com e sem 'Meeple's
{-validNextTilesWithMeeples :: Board -> [Tile]
validNextTilesWithMeeples board = validTiles ++ tilesComMeeples
                           where validType = getNextTileType board
                                 validTiles = filter ((validType==).t_type) tiles
                                 tilesComMeeples = if nextPlayerHasMeeples board then getTilesWithMeeples board validTiles else []
                                 tiles = possibleNextTiles board-}

-- verificar a peça seguinte e filtrar as possiveis
-- verificar se o jogador pode usar meeples
-- | Obter os 'Tile's que podem ser jogados
{-validNextTiles :: Board -> [Tile]
validNextTiles board = validTiles
                       where validTiles = filter ((validType==).t_type) 
                             tiles = possibleNextTiles board-}

-- verificar a peça seguinte e filtrar as possiveis
-- verificar se o jogador pode usar meeples
-- | Obter um tile aletório para a próxima jogada
randomValidNextTile :: Int -> Board -> Tile
randomValidNextTile seed board = (todos !! (randomValue seed ((length todos)-1)))
                    where validType = getNextTileType board
                          validTiles = filter ((validType==).t_type) tiles
                          tilesComMeeples = if ((randomValue seed 1) == 1) && nextPlayerHasMeeples board then getTilesWithMeeples board validTiles else []
                          tiles = possibleNextTiles board
                          todos = validTiles ++ tilesComMeeples
                                
-- seed deve ser entre 0 e 1000
randomValue :: Int -> Int -> Int
randomValue seed maximo = seed `mod` (maximo+1) 


-- | Gera um valor pseudo-aleatório no intervalo [0; maximo]
pseudoRandomValue :: [Tile] -> Int -> Int
pseudoRandomValue tiles maximo = (foldr (+) 0 (map (\x -> (t_x x) + (t_y x) + (ord (t_type x)) + (ord (t_orientation x))) tiles)) `mod` (maximo+1)

-- | A partir do tabuleiro e da lista de 'Tile's que podem ser colocados, devolver uma lista de 'Tile's que podem ser colocados com e sem 'Meeples's
getTilesWithMeeples :: Board -> [Tile] -> [Tile]
getTilesWithMeeples board tiles = knights ++ farmers ++ monks
                            where knights = getTilesFromMaybeTiles (map (getTileWithKnight board) tiles)
                                  farmers = getTilesFromMaybeTiles (map (getTileWithFarmer board) tiles)
                                  monks = getTilesFromMaybeTiles (map (getTileWithMonk) tiles)

-- | Verificar todos os tile de cidade à volta a procura de um outro meeple, se nao existir, pode colocar
getTileWithKnight :: Board -> Tile -> Maybe Tile
getTileWithKnight board tile =  if (tipo=='C'||tipo=='E'||tipo=='N') && (canThisTileHaveMeeple tile board 'K')
                                then Just (Tile { t_type = tipo
                                                , t_x = (t_x tile)
                                                , t_y = (t_y tile)
                                                , t_orientation = (t_type tile)
                                                , t_meeple = Just (Meeple {m_player=0,m_type='K'})
                                                })
                                else Nothing
                                where tipo = t_type tile

-- | Verificar todos os tile de field à volta a procura de um outro meeple, se nao existir, pode colocar
getTileWithFarmer :: Board -> Tile -> Maybe Tile
getTileWithFarmer board tile =  if (tipo=='B'||tipo=='E'||tipo=='N') && (canThisTileHaveMeeple tile board 'F')
                                then Just (Tile { t_type = tipo
                                                , t_x = (t_x tile)
                                                , t_y = (t_y tile)
                                                , t_orientation = (t_type tile)
                                                , t_meeple = Just (Meeple {m_player=0,m_type='F'})
                                                })
                                else Nothing
                                where tipo = t_type tile

-- | Verificar todos os tile de field à volta a procura de um outro meeple, se nao existir, pode colocar
getTileWithMonk :: Tile -> Maybe Tile
getTileWithMonk tile =  if (t_type tile) == 'B'
                        then Just (Tile { t_type = 'B'
                                        , t_x = (t_x tile)
                                        , t_y = (t_y tile)
                                        , t_orientation = (t_type tile)
                                        , t_meeple = Just (Meeple {m_player=0,m_type='M'})
                                        })
                        else Nothing

-- | Verifica se um tile pode ter um meeple do tipo definido
canThisTileHaveMeeple :: Tile -> Board -> Char -> Bool
canThisTileHaveMeeple newTile board meepleType = searchMeeple meepleType tiles [newTile] []
                                       where tiles = b_terrain board

-- todos, porVer, vistos
-- verificar se o proprio tem meeple, se tiver return false
-- verificar quais dos tiles adicionar ao por ver com base nas bordas e no tipo de meeple
-- | Procurar 'Meeples's
searchMeeple :: Char -> [Tile] -> [Tile] -> [Tile] -> Bool
searchMeeple _ _ [] _ = True
searchMeeple meeple todos (this:porVer) vistos = 
    if isJust (t_meeple this)
    then False -- encontrou um meeple, não se pode colocar nenhum meeple no tile original
    else searchMeeple meeple todos (porVer ++ filter (\x -> x `elem` vistos) (tilesToAddBasedOnMeepleType meeple this todos)) (this:vistos)

-- | Encontrar os tiles adjacentes que podem ter meeples do mesmo tipo
tilesToAddBasedOnMeepleType :: Char -> Tile -> [Tile] -> [Tile]
tilesToAddBasedOnMeepleType 'K' tile tiles = getTilesFromMaybeTiles (map (getTileAtLocation tiles) (getFollowingTilesPositions tile City (getSidesFromTile tile)))
tilesToAddBasedOnMeepleType 'F' tile tiles = getTilesFromMaybeTiles (map (getTileAtLocation tiles) (getFollowingTilesPositions tile City (getSidesFromTile tile)))

-- | Obtém uma lista das posições dos 'Tile's por onde se deve continuar a procurar 'Meeple's
getFollowingTilesPositions :: Tile -> Side -> Sides -> [Location]
getFollowingTilesPositions tile City sides = case sides of
        (City,b,c,d) -> (t_x tile, t_y tile+1):(getFollowingTilesPositions tile City (Both,b,c,d))
        (a,City,c,d) -> (t_x tile+1, t_y tile):(getFollowingTilesPositions tile City (a,Both,c,d))
        (a,b,City,d) -> (t_x tile, t_y tile-1):(getFollowingTilesPositions tile City (a,b,Both,d))
        (a,b,c,City) -> (t_x tile-1, t_y tile):(getFollowingTilesPositions tile City (a,b,c,Both))
        _ -> []
getFollowingTilesPositions tile Field sides = case sides of
        (Field,b,c,d) -> (t_x tile, t_y tile+1):(getFollowingTilesPositions tile Field (Both,b,c,d))
        (a,Field,c,d) -> (t_x tile+1, t_y tile):(getFollowingTilesPositions tile Field (a,Both,c,d))
        (a,b,Field,d) -> (t_x tile, t_y tile-1):(getFollowingTilesPositions tile Field (a,b,Both,d))
        (a,b,c,Field) -> (t_x tile-1, t_y tile):(getFollowingTilesPositions tile Field (a,b,c,Both))
        _ -> []

--------------------------------------------
--------------------------------------------

pecasRestantes :: Board -> (Int,Int,Int,Int)
pecasRestantes = restantes . contadorPecas . b_terrain
                 where restantes (b,c,e,n) = (nPecasB-b, nPecasC-c, nPecasE-e, nPecasN-n)

contadorPecas :: [Tile] -> (Int,Int,Int,Int)
contadorPecas [] = (0,0,0,0)
contadorPecas (x:xs) =
              case t_type x of
                'B' -> (b+1,c,e,n)
                'C' -> (b,c+1,e,n)
                'E' -> (b,c,e+1,n)
                'N' -> (b,c,e,n+1)
              where (b,c,e,n) = contadorPecas xs