-- | Contém funções para alterar e obter diversas informações sobre o tabuleiro
module Tabuleiro where

import Leitor
import Data.List
import Data.Maybe
import Data.Char


import FakePrettyShow
--import FakePrettyShow

import Debug.Trace

-- | Representa uma zona do mapa: uma cidade, um campo ou um claustro, começando num determinado 'Tile'.
-- | O 'Tile' inicial também deve estar na lista.
data Zone = City Tile [Tile] | Field Tile [Tile] | Cloister Tile [Tile]

-- | Números de peças do tipo B que podem ser jogadas
nPecasB = 6 :: Int

-- | Números de peças do tipo C que podem ser jogadas
nPecasC = 2 :: Int

-- | Números de peças do tipo E que podem ser jogadas
nPecasE = 18 :: Int

-- | Números de peças do tipo N que podem ser jogadas
nPecasN = 10 :: Int

-- | Números de peças que podem ser jogadas (no total)
nPecasT = nPecasB + nPecasC + nPecasE + nPecasN :: Int

-- | Representa a lateral de uma peça
data Side = SideField | SideCity | SideBoth
    deriving (Eq, Show)

-- | Representa as quatro laterais de uma peça
type Sides = (Side, Side, Side, Side)

-- | Remove duplicados de uma lista
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\vistos x -> if x `elem` vistos then vistos else vistos ++ [x]) []

-- | Obtém o tile que está numa posição, ou Nothing caso não exista nenhum nessa posição
getTileAtLocation :: [Tile] -> Location -> Maybe Tile
getTileAtLocation [] _ = Nothing
getTileAtLocation (tile@(Tile {t_x=tx, t_y=ty}):t) loc@(x,y) = if tx==x && ty==y then Just tile
                                                               else getTileAtLocation t loc

-- | Obtém os valores máximos e mínimos para as coordenadas X e Y
getLimits :: Board -> Limits
getLimits (Board {b_terrain=tiles}) = Limits { l_Xmin = xmin
                                             , l_Xmax = xmax
                                             , l_Ymin = ymin
                                             , l_Ymax = ymax
                                             }
                                         where (xmin, xmax) = getTileLimit (t_x) tiles
                                               (ymin, ymax) = getTileLimit (t_y) tiles

-- | Obtém o valor máximo para uma das coordenadas X ou Y, especificadas como 't_x' ou 't_y'
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
getSidesFromTypeAndOrientation 'B' _ = (SideField, SideField, SideField, SideField)
getSidesFromTypeAndOrientation 'C' _ = (SideCity, SideCity, SideCity, SideCity)
getSidesFromTypeAndOrientation 'E' o = case o of
                                            'N' -> (SideCity, SideField, SideField, SideField)
                                            'E' -> (SideField, SideCity, SideField, SideField)
                                            'S' -> (SideField, SideField, SideCity, SideField)
                                            'W' -> (SideField, SideField, SideField, SideCity)
getSidesFromTypeAndOrientation 'N' o = case o of
                                            'N' -> (SideCity, SideField, SideField, SideCity)
                                            'E' -> (SideCity, SideCity, SideField, SideField)
                                            'S' -> (SideField, SideCity, SideCity, SideField)
                                            'W' -> (SideField, SideField, SideCity, SideCity)

-- | Informa que tipo de tile pode estar na posição (x,y)
possibleBordersOfTileAt :: Int -> Int -> [Tile] -> Sides
possibleBordersOfTileAt x y tiles = (southBorder (borderTypeMaybeTile up), westBorder (borderTypeMaybeTile right), northBorder (borderTypeMaybeTile down), eastBorder (borderTypeMaybeTile left))
                                    where up = getTileAtLocation tiles (x,y+1)
                                          left = getTileAtLocation tiles (x-1,y)
                                          down = getTileAtLocation tiles (x,y-1)
                                          right = getTileAtLocation tiles (x+1,y)

-- | Dado um 'Tile' obtém as suas bordas
borderTypeMaybeTile :: Maybe Tile -> Sides
borderTypeMaybeTile Nothing = (SideBoth,SideBoth,SideBoth,SideBoth)
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

-- | Desdobra 'Sides' com lados ambíguos ('SideBoth') numa lista de 'Sides' que apenas são 'Field' ou 'SideCity'
removeAmbiguousSides :: Sides -> [Sides]
removeAmbiguousSides (SideBoth, b, c, d) = (SideCity,b,c,d):(SideField,b,c,d):( (removeAmbiguousSides (SideCity,b,c,d)) ++ (removeAmbiguousSides (SideField,b,c,d)))
removeAmbiguousSides (a, SideBoth, c, d) = (a,SideCity,c,d):(a,SideField,c,d):( (removeAmbiguousSides (a,SideCity,c,d)) ++ (removeAmbiguousSides (a,SideField,c,d)))
removeAmbiguousSides (a, b, SideBoth, d) = (a,b,SideCity,d):(a,b,SideField,d):( (removeAmbiguousSides (a,b,SideCity,d)) ++ (removeAmbiguousSides (a,b,SideField,d)))
removeAmbiguousSides (a, b, c, SideBoth) = (a,b,c,SideCity):(a,b,c,SideField):( (removeAmbiguousSides (a,b,c,SideCity)) ++ (removeAmbiguousSides (a,b,c,SideField)))
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
    (SideField, SideField, SideField, SideField) -> [Just (Tile { t_type = 'B', t_x = x, t_y = y, t_orientation = o, t_meeple = Nothing}) | o <- ['N','E','S','W']]
    (SideCity, SideCity, SideCity, SideCity) -> [Just (Tile { t_type = 'C', t_x = x, t_y = y, t_orientation = o, t_meeple = Nothing}) | o <- ['N','E','S','W']]
    (SideCity, SideField, SideField, SideField) -> [Just (Tile { t_type = 'E', t_x = x, t_y = y, t_orientation = 'N', t_meeple = Nothing})]
    (SideField, SideCity, SideField, SideField) -> [Just (Tile { t_type = 'E', t_x = x, t_y = y, t_orientation = 'E', t_meeple = Nothing})]
    (SideField, SideField, SideCity, SideField) -> [Just (Tile { t_type = 'E', t_x = x, t_y = y, t_orientation = 'S', t_meeple = Nothing})]
    (SideField, SideField, SideField, SideCity) -> [Just (Tile { t_type = 'E', t_x = x, t_y = y, t_orientation = 'W', t_meeple = Nothing})]
    (SideCity, SideField, SideField, SideCity) -> [Just (Tile { t_type = 'N', t_x = x, t_y = y, t_orientation = 'N', t_meeple = Nothing})]
    (SideCity, SideCity, SideField, SideField) -> [Just (Tile { t_type = 'N', t_x = x, t_y = y, t_orientation = 'E', t_meeple = Nothing})]
    (SideField, SideCity, SideCity, SideField) -> [Just (Tile { t_type = 'N', t_x = x, t_y = y, t_orientation = 'S', t_meeple = Nothing})]
    (SideField, SideField, SideCity, SideCity) -> [Just (Tile { t_type = 'N', t_x = x, t_y = y, t_orientation = 'W', t_meeple = Nothing})]
    _ -> [Nothing]


-- | Dado uma localização e a lista de 'Tile' em jogo, informa que peças podem ser lá colocadas
possibleTilesAt :: [Tile] -> Location -> [Tile]
possibleTilesAt tiles (x,y) =
  case getTileAtLocation tiles (x,y) of
    Nothing -> case (possibleBordersOfTileAt x y tiles) of
                 (SideBoth, SideBoth, SideBoth, SideBoth) -> [] -- não há nenhum tile adjacente
                 sides -> getTilesFromMaybeTiles (getTilesFromSides x y (removeDuplicates (removeAmbiguousSides sides)))
    (Just _) -> []

-- | A partir do 'Board' descobre todas as peças que podem ser colocadas, onde e em que posição (isto ignorando meeples e tendo em conta o número máximo de peças jogáveis de um determinado tipo)
possibleNextTiles :: Board -> [Tile]
possibleNextTiles b = trace ("PecasRestantes(B,C,E,N): " ++ show (pecasRestantes b)) filter (aindaPodemSerColocadasPecasDesteTipo.t_type) todos
  where limits = (getLimits b)
        tiles = b_terrain b
        (xmin,xmax,ymin,ymax) = ((l_Xmin limits)-1, (l_Xmax limits)+1, (l_Ymin limits)-1, (l_Ymax limits)+1)
        positions = concat [[(x,y) | x <- [xmin..xmax]] | y <- [ymax, (ymax-1)..ymin] ]
        todos = concat (map (possibleTilesAt tiles) positions)
        (rB,rC,rE,rN) = pecasRestantes b
        aindaPodemSerColocadasPecasDesteTipo 'B' = rB > 0
        aindaPodemSerColocadasPecasDesteTipo 'C' = rC > 0
        aindaPodemSerColocadasPecasDesteTipo 'E' = rE > 0
        aindaPodemSerColocadasPecasDesteTipo 'N' = rN > 0




-- | Obter o tipo do próximo tile
getNextTileType :: Board -> TileType
getNextTileType (Board {b_next=proximaJogada}) = n_tile proximaJogada

-- | Obter o próximo jogador (aquele que deve jogar nesta fase play)
getNextPlayer :: Board -> Player
getNextPlayer (Board tiles players _) = players !! proxJogadorIndx
              where proxJogadorIndx = mod nTiles nJogadores
                    nJogadores = length players
                    nTiles = length tiles

-- | Verifica se o próximo jogador tem meeples por jogar
nextPlayerHasMeeples :: Board -> Bool
nextPlayerHasMeeples board = (meeplesPlacedByNextPlayer board) <= 7

-- | Verifica quantos meeples o próximo jogador tem em jogo
meeplesPlacedByNextPlayer :: Board -> Int
meeplesPlacedByNextPlayer board = length (filter ((nPlayer==).m_player) (map (fromJust) (filter (isJust) (map (t_meeple) (b_terrain board)))))
                                  where nPlayer = s_player (getNextPlayer board)

-- verificar a peça seguinte e filtrar as possiveis
-- verificar se o jogador pode usar meeples
-- Obter os 'Tile's que podem ser jogados, com e sem 'Meeple's
{-validNextTilesWithMeeples :: Board -> [Tile]
validNextTilesWithMeeples board = validTiles ++ tilesComMeeples
                           where validType = getNextTileType board
                                 validTiles = filter ((validType==).t_type) tiles
                                 tilesComMeeples = if nextPlayerHasMeeples board then getTilesWithMeeples board validTiles else []
                                 tiles = possibleNextTiles board-}

-- verificar a peça seguinte e filtrar as possiveis
-- verificar se o jogador pode usar meeples
-- Obter os 'Tile's que podem ser jogados
{-validNextTiles :: Board -> [Tile]
validNextTiles board = validTiles
                       where validTiles = filter ((validType==).t_type) 
                             tiles = possibleNextTiles board-}


-- | Gera um elemento 'Next' para a próxima jogada
generateNext :: Int -> Board -> Next
generateNext seed board = Next (t_type (todos !! (randomValue seed ((length todos)-1))))
            where todos = possibleNextTiles board

                                
-- | Fornecendo uma seed entre 0 e 1000, produz um número entre 0 e o segundo argumento.
randomValue :: Int -> Int -> Int
randomValue seed maximo = trace ("maxRandom: " ++ show maximo) (seed `mod` (maximo+1))

-- | A partir do tabuleiro e da lista de 'Tile's que podem ser colocados, devolver uma lista de 'Tile's com 'Meeples's que podem ser colocados.
getTilesWithMeeples :: Board -> [Tile] -> [Tile]
getTilesWithMeeples board tiles = knights ++ farmers ++ monks
                            where knights = getTilesFromMaybeTiles (map (getTileWithKnight board owner) tiles)
                                  farmers = getTilesFromMaybeTiles (map (getTileWithFarmer board owner) tiles)
                                  monks = getTilesFromMaybeTiles (map (getTileWithMonk owner) tiles)
                                  owner = s_player $ getNextPlayer board

-- | Verificar todos os tile de cidade à volta a procura de um outro meeple, se nao existir, pode colocar
getTileWithKnight :: Board -> Int -> Tile -> Maybe Tile
getTileWithKnight board owner tile =  if (tipo=='C'||tipo=='E'||tipo=='N') && (canThisTileHaveMeeple tile board 'K')
                                      then Just (Tile { t_type = tipo
                                                      , t_x = (t_x tile)
                                                      , t_y = (t_y tile)
                                                      , t_orientation = (t_orientation tile)
                                                      , t_meeple = Just (Meeple {m_player=owner,m_type='K'})
                                                      })
                                      else Nothing
                                      where tipo = t_type tile

-- | Verificar todos os tile de field à volta a procura de um outro meeple, se nao existir, pode colocar
getTileWithFarmer :: Board -> Int -> Tile -> Maybe Tile
getTileWithFarmer board owner tile =  if (tipo=='B'||tipo=='E'||tipo=='N') && (canThisTileHaveMeeple tile board 'F')
                                      then Just (Tile { t_type = tipo
                                                      , t_x = (t_x tile)
                                                      , t_y = (t_y tile)
                                                      , t_orientation = (t_orientation tile)
                                                      , t_meeple = Just (Meeple {m_player=owner,m_type='F'})
                                                      })
                                      else Nothing
                                      where tipo = t_type tile

-- | Verificar todos os tile de field à volta a procura de um outro meeple, se nao existir, pode colocar
getTileWithMonk :: Int -> Tile -> Maybe Tile
getTileWithMonk owner tile = if (t_type tile) == 'B'
                            then Just (Tile { t_type = 'B'
                                            , t_x = (t_x tile)
                                            , t_y = (t_y tile)
                                            , t_orientation = (t_orientation tile)
                                            , t_meeple = Just (Meeple {m_player=owner,m_type='M'})
                                            })
                            else Nothing

-- | Verifica se um tile pode ter um meeple do tipo definido
canThisTileHaveMeeple :: Tile -> Board -> Char -> Bool
canThisTileHaveMeeple newTile board meepleType = searchMeeple meepleType tiles [newTile] []
                      where tiles = b_terrain board

-- todos, porVer, vistos
-- verificar se o proprio tem meeple, se tiver return false
-- verificar quais dos tiles adicionar ao por ver com base nas bordas e no tipo de meeple
-- | Procurar 'Meeple's
searchMeeple :: Char -> [Tile] -> [Tile] -> [Tile] -> Bool
searchMeeple _ _ [] _ = True
searchMeeple meeple todos (this:porVer) vistos = 
    if isJust (t_meeple this) && (m_type $ fromJust (t_meeple this)) == meeple
    then False -- encontrou um meeple, não se pode colocar nenhum meeple no tile original
    else searchMeeple meeple todos (porVer ++ novos) (this:vistos)
    where novos = filter (\x -> not $ x `elem` (this:vistos++porVer)) (tilesToAddBasedOnMeepleType meeple this todos)

-- | Encontrar os tiles adjacentes que podem ter meeples do mesmo tipo
tilesToAddBasedOnMeepleType :: Char -> Tile -> [Tile] -> [Tile]
tilesToAddBasedOnMeepleType 'K' tile tiles = getTilesFromMaybeTiles (map (getTileAtLocation tiles) (getFollowingTilesPositions tile SideCity (getSidesFromTile tile)))
tilesToAddBasedOnMeepleType 'F' tile tiles = getTilesFromMaybeTiles (map (getTileAtLocation tiles) (getFollowingTilesPositions tile SideField (getSidesFromTile tile)))

-- | Obtém uma lista das posições dos 'Tile's por onde se deve continuar a procurar 'Meeple's
getFollowingTilesPositions :: Tile -> Side -> Sides -> [Location]
getFollowingTilesPositions tile SideCity sides = case sides of
        (SideCity,b,c,d) -> (t_x tile, t_y tile+1):(getFollowingTilesPositions tile SideCity (SideBoth,b,c,d))
        (a,SideCity,c,d) -> (t_x tile+1, t_y tile):(getFollowingTilesPositions tile SideCity (a,SideBoth,c,d))
        (a,b,SideCity,d) -> (t_x tile, t_y tile-1):(getFollowingTilesPositions tile SideCity (a,b,SideBoth,d))
        (a,b,c,SideCity) -> (t_x tile-1, t_y tile):(getFollowingTilesPositions tile SideCity (a,b,c,SideBoth))
        _ -> []
getFollowingTilesPositions tile SideField sides = case sides of
        (SideField,b,c,d) -> (t_x tile, t_y tile+1):(getFollowingTilesPositions tile SideField (SideBoth,b,c,d))
        (a,SideField,c,d) -> (t_x tile+1, t_y tile):(getFollowingTilesPositions tile SideField (a,SideBoth,c,d))
        (a,b,SideField,d) -> (t_x tile, t_y tile-1):(getFollowingTilesPositions tile SideField (a,b,SideBoth,d))
        (a,b,c,SideField) -> (t_x tile-1, t_y tile):(getFollowingTilesPositions tile SideField (a,b,c,SideBoth))
        _ -> []

-- | verifica se uma cidade está completa
isCityComplete :: [Tile] -> [Tile] -> [Tile] -> Bool
isCityComplete _ [] _ = True
isCityComplete todos (this:porVer) vistos =
    -- este tile tem uma lateral de cidade que não tem tile à volta
    if not $ null (filter (isNothing) (map (getTileAtLocation todos) (getFollowingTilesPositions this SideCity (getSidesFromTile this))))
    then False
    else isCityComplete todos (porVer ++ filter (\x -> not $ x `elem` (this:vistos++porVer)) (tilesToAddBasedOnMeepleType 'K' this todos)) (this:vistos)

--------------------------------------------
--------------------------------------------

-- | Verifica quantas peças de cada tipo (B,C,E,N) estão por colocar
pecasRestantes :: Board -> (Int,Int,Int,Int)
pecasRestantes = restantes . contadorPecas . b_terrain
                 where restantes (b,c,e,n) = (nPecasB-b, nPecasC-c, nPecasE-e, nPecasN-n)

-- | Verifica quantas peças de cada tipo (B,C,E,N) estão colocadas
contadorPecas :: [Tile] -> (Int,Int,Int,Int)
contadorPecas [] = (0,0,0,0)
contadorPecas (x:xs) =
              case t_type x of
                'B' -> (b+1,c,e,n)
                'C' -> (b,c+1,e,n)
                'E' -> (b,c,e+1,n)
                'N' -> (b,c,e,n+1)
              where (b,c,e,n) = contadorPecas xs

{-- | Obter um tile aletório para a próxima jogada
randomValidTileToPlay :: Int -> Board -> Tile
randomValidTileToPlay seed board = (validTiles !! (randomValue seed ((length validTiles)-1)))
                    where validTiles = filter (pertenceAoTipoCerto) tiles
                          tiles = possibleNextTiles board
                          restantes = (listOfTypesFromTuple . contadorPecas . b_terrain) board
                          pertenceAoTipoCerto t = (t_type t) `elem` restantes
-}

-- | Obter um tile aletório para jogar daqueles que podem ser 
randomValidTileToPlay :: Int -> Board -> Tile
randomValidTileToPlay seed board = res
                    where tiles = possibleNextTiles board
                          tipoCerto = n_tile.b_next $ board
                          validTiles = filter ((tipoCerto==).t_type) tiles
                          validTilesComMeeples = if ((randomValue seed 1) == 1) && nextPlayerHasMeeples board then getTilesWithMeeples board validTiles else []
                          todos = validTiles ++ validTilesComMeeples
                          res = (todos !! (randomValue seed ((length todos)-1)))
                          --debug = ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\nEscolhido: " ++ show res

-- | Jogar a primeira peça
playFirstTile :: Int -> Board -> Tile
playFirstTile seed board = Tile 'E' 0 0 theOrientation theMeeple
              where firstPlayer = s_player (getNextPlayer board)
                    theMeeple = case (randomValue seed 9) of
                                    0 -> Nothing -- com 10% de prob
                                    1 -> Just (Meeple firstPlayer 'F') -- com 10% de prob
                                    _ -> Just (Meeple firstPlayer 'K') -- com 80% de prob
                    theOrientation = case (randomValue seed 3) of -- cada um com 25% de prob
                                    0 -> 'N'
                                    1 -> 'E'
                                    2 -> 'S'
                                    3 -> 'W'

-- | Obter uma lista de 'TileType' a partir de um tuplo com as quantidades de peças (B,C,E,N)
listOfTypesFromTuple :: (Int,Int,Int,Int) -> [TileType]
listOfTypesFromTuple (0,0,0,0) = []
listOfTypesFromTuple (b,c,e,n) = lb ++ lc ++ le ++ ln
                      where lb = if b > 0 then "B" else []
                            lc = if c > 0 then "C" else []
                            le = if e > 0 then "E" else []
                            ln = if n > 0 then "N" else []

-- | Verifica se o jogo terminou
isGameOver :: Board -> Bool
isGameOver b = trace ("Nr restantes possiveis: " ++ (show.length) (possibleNextTiles b)) $ (null.possibleNextTiles) b

-- | Substitui a componente 'Next' de um 'Board'
substituteNext :: Board -> Next -> Board
substituteNext b n = Board { b_terrain = b_terrain b
                           , b_scores = b_scores b
                           , b_next = n}

-- | Obtém todos os 'Tile's que têm 'Meeple's
getAllTilesWithMeeples :: Board -> [Tile]
getAllTilesWithMeeples b = filter (hasMeeple) (b_terrain b)
                        where hasMeeple tile = isJust $ t_meeple tile

-- | Verifica se duas 'Zone's se referem à mesma região
isSameZone :: Zone -> Zone -> Bool
isSameZone (City x xs) (City y ys) = (tilesAreTheSame x y) || (tilesListsAreTheSame (xs) (ys))
isSameZone (Field x xs) (Field y ys) = (tilesAreTheSame x y) || (tilesListsAreTheSame (xs) (ys))
isSameZone (Cloister x xs) (Cloister y ys) = (tilesAreTheSame x y) || (tilesListsAreTheSame (xs) (ys))
isSameZone _ _ = False

-- | Verifica se duas lista de 'Tile's contêm os mesmos elementos
tilesListsAreTheSame :: [Tile] -> [Tile] -> Bool
tilesListsAreTheSame t1s t2s = and (map (belongsTo t2s) t1s) && and (map (belongsTo t1s) t2s)
                      where belongsTo (l:ls) tile = if tilesAreTheSame l tile then True else belongsTo ls tile
                            belongsTo [] _ = False

-- | Verifica se dois 'Tile's têm a mesma localização
tilesAreTheSame :: Tile -> Tile -> Bool
tilesAreTheSame a b = (t_x a)==(t_x b) && (t_y a)==(t_y b)

-- | Recebe um 'Tile' com 'Meeple' e obtém a zona dominada por esse 'Meeple'
getZoneForMeeple :: Board -> Tile -> Zone
getZoneForMeeple b tile = 
        case meepleType of
          'M' -> Cloister tile (getCloisterTiles tile tiles)
          'K' -> City tile (getCityTiles tile tiles)
          'F' -> Field tile (getFieldTiles tile tiles)
        where meepleType = (m_type.fromJust.t_meeple) tile
              tiles = b_terrain b

-- | Obtém os 'Tile's de uma cidade começando no 'Tile' especificado
getCityTiles :: Tile -> [Tile] -> [Tile]
getCityTiles start l = aux l [start] []
            where aux todos (this:porVer) vistos = (aux todos (porVer ++ filter (\x -> not $ x `elem` verificados) (tilesToAddBasedOnMeepleType 'K' this todos)) (this:vistos))
                      where verificados = this:vistos++porVer
                  aux _ [] vistos = vistos

-- | Obtém os 'Tile's de um campo começando no 'Tile' especificado
getFieldTiles :: Tile -> [Tile] -> [Tile]
getFieldTiles start l = aux l [start] []
            where aux todos (this:porVer) vistos = (aux todos (porVer ++ filter (\x -> not $ x `elem` verificados) (tilesToAddBasedOnMeepleType 'F' this todos)) (this:vistos))
                      where verificados = this:vistos++porVer
                  aux _ [] vistos = vistos

-- | obtém os 'Tile's junto ao claustro e o 'Tile' do próprio claustro
getCloisterTiles :: Tile -> [Tile] -> [Tile]
getCloisterTiles start l = start:(map fromJust (filter isJust (map (getTileAtLocation l) locations)))
                  where locations = [(x-1,y+1), (x,y+1), (x+1,y+1), (x-1,y), (x+1,y), (x-1,y-1), (x,y-1), (x+1,y-1)]
                        x = t_x start
                        y = t_y start

-- | Acrescenta o 'Tile' ao 'Board'
addTileToBoard :: Board -> Tile -> Board
addTileToBoard b t = Board ( t:(b_terrain b)) (b_scores b) (b_next b)