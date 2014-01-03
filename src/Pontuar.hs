-- | Trata de contar os pontos
module Pontuar where

import Text.XML.Light
import Tabuleiro
import Leitor
import Data.Maybe
import Data.List

import Debug.Trace

-- | Representa uma 'Zone' e a sua pontuação
type ScoredTile = (Zone, Int)

-- | Obtém o 'Tile' inicial de uma 'Zone'
getFirstTile :: Zone -> Tile
getFirstTile (City x _) = x
getFirstTile (Field x _) = x
getFirstTile (Cloister x _) = x

-- | Converte uma lista de 'ScoredTile' para uma String legível
scoredTiles2String :: [ScoredTile] -> String
scoredTiles2String l = unlines $ map mostrar l
        where inicial (City x l) = (show x):(map show l)
              inicial (Field x l) = (show x):(map show l)
              inicial (Cloister x l) = (show x):(map show l)
              mostrar (zona, p) = show p ++ " pontos para o tile: " ++ unlines (inicial zona)


-- | Calcula as pontuações, inclusive dos Farmers, retira todos os meeples e atribui pontuações finais
finalScore :: Board -> Board
--finalScore b = trace (scoredTiles2String realScores) (Board {b_terrain=tilesWithoutMeeples, b_scores=scoredPlayers, b_next=(Next '-')})
finalScore b = Board tilesWithoutMeeples scoredPlayers (Next '-')
            where players = b_scores b
                  realScores = getRealScoresForZones b $ map (\x->(getZoneForMeeple b x,0)) (getAllTilesWithMeeples b)
                  scoredPlayers = scorePlayers realScores players
                  tilesWithoutMeeples = removeMeeples realScores (b_terrain b)
-- map (getZoneForMeeple b) (getAllTilesWithMeeples b) -- Obtém 'ScoredTile's que devem ser pontuados

-- | Retira os meeples e atribui pontuações
updateScore :: Board -> Board
updateScore b = (Board {b_terrain=tilesWithoutMeeples, b_scores=scoredPlayers, b_next=(b_next b)})
            where players = b_scores b
                  realScores = getRealScoresForZones b $ obterMeeplesPorRetirar b
                  scoredPlayers = scorePlayers realScores players
                  tilesWithoutMeeples = removeMeeples realScores (b_terrain b)

-- | Alterar os scores dos jogadores
scorePlayers :: [ScoredTile] -> [Player] -> [Player]
scorePlayers _ [] = []
scorePlayers scores (p:ps) = (Player {s_player=(s_player p), s_score=(s_score p)+playerScore}):(scorePlayers scores ps)
              where belongsToPlayer st = ((m_player.fromJust.t_meeple.getFirstTile.fst) st)==(s_player p)
                    playerScore = sum $ map snd (filter (belongsToPlayer) scores)

-- | Produz uma nova lista de 'Tile's sem os 'Meeple's que já foram pontuados
removeMeeples :: [ScoredTile] -> [Tile] -> [Tile]
removeMeeples _ [] = []
removeMeeples stiles (t:ts) = newTile:(removeMeeples stiles ts)
              where shouldRemove = isJust $ find (\st -> (getFirstTile $ fst st)==t) stiles
                    newTile = if shouldRemove then (Tile {t_type = t_type t, t_x = t_x t, t_y = t_y t, t_orientation = t_orientation t, t_meeple = Nothing}) else t


-- | Obtém 'ScoredTile's que devem ser pontuados nesta jogada
obterMeeplesPorRetirar :: Board -> [ScoredTile]
obterMeeplesPorRetirar b = monks ++ knights
                      where tilesWithMeeples = getAllTilesWithMeeples b
                            monks = map (getScoredTileFromTile b) (filter (isMonkDone b) tilesWithMeeples)
                            knights = map (getScoredTileFromTile b) (filter (isKnightDone b) tilesWithMeeples)

-- | Obtém um 'ScoredTile' a partir de um 'Tile'. A pontuação inicial é 0.
getScoredTileFromTile :: Board -> Tile -> ScoredTile
getScoredTileFromTile b t = (zone, 0)
                      where zone = getZoneForMeeple b t

-- | Pontuar uma 'Zone'
scoreZone :: Zone -> Board -> Int
scoreZone (Cloister _ tiles) _ = (length tiles)
scoreZone (City inicial tiles) _ = (if isCityComplete tiles [inicial] [] then 2 else 1) * (length tiles)
scoreZone field b = scoreField field b

-- | Pontuar um 'Field'
-- para obter numero de cidades completas alcançáveis:

-- obter todos os tiles de cidade alcancaveis - OK
-- para cada um deles calcular a zone - OK
-- remover duplicados - OK
-- ver quantos desses são cidades completas - OK
-- multiplicar isso por 3 - OK
scoreField :: Zone -> Board -> Int
scoreField (Field inicial tiles) b = (3*) $ length $ filter (completa) $ removeDuplicateCities $ map (getZoneForMeeple b) $ map (colocarMeeple) $ filter (hasCity) tiles
          where hasCity (Tile 'E' _ _ _ _) = True
                hasCity (Tile 'N' _ _ _ _) = True
                hasCity _ = False
                colocarMeeple (Tile a b c d Nothing) = (Tile a b c d (Just (Meeple (-1) 'K')))
                colocarMeeple tile = tile
                completa (City i todos) = isCityComplete (todos) [i] []

-- | Remove cidades duplicadas
removeDuplicateCities :: [Zone] -> [Zone]
removeDuplicateCities = foldl (\vistos x -> if x `pertence` vistos then vistos else vistos ++ [x]) []
                where pertence (City inicial todos) lista = isJust $ find (tilesListsAreTheSame todos) (map getTiles lista)
                      getTiles (City _ todos) = todos

-- | Contar o número de meeples numa 'City' do mesmo dono do meeple inicial
countMeeplesInCity :: Zone -> Int
countMeeplesInCity (City inicial tiles) = length.(filter (mesmoDono)) $ tiles
  where mesmoDono tile = dono == getDono tile
        getDono (Tile _ _ _ _ (Just m)) = m_player m
        dono = getDono inicial

-- | Verifica se o 'Meeple' monk deve ser retirado do tabuleiro
isMonkDone :: Board -> Tile -> Bool
isMonkDone b tMeeple = (m_type meeple) == 'M' && existsTile (x-1,y+1) && existsTile (x,y+1) && existsTile (x+1,y+1) && existsTile (x-1,y) && existsTile (x+1,y) && existsTile (x-1,y-1) && existsTile (x,y-1) && existsTile (x+1,y-1)
          where meeple = fromJust.t_meeple $ tMeeple
                tiles = b_terrain b
                x = t_x tMeeple
                y = t_y tMeeple
                existsTile location = isJust $ getTileAtLocation tiles location

-- | Verifica se o 'Meeple' knight deve ser retirado do tabuleiro
isKnightDone :: Board -> Tile -> Bool
isKnightDone b tMeeple = (m_type meeple) == 'K' && isCityComplete tiles [tMeeple] []
            where meeple = fromJust.t_meeple $ tMeeple
                  tiles = b_terrain b
                  x = t_x tMeeple
                  y = t_y tMeeple

-- scoreAZone :: [ScoredTile] -> [Int] -> [ScoredTile]
-- | Obtém o verdadeiro score associado a uma zona depois de contar o número de 'Meeple's presente nessa zona
getRealScoresForZones :: Board -> [ScoredTile] -> [ScoredTile]
getRealScoresForZones b zones = concat $ zipWith (scoreAZone) groupedZones playersToBeScored
                        where groupedZones = joinScoredTilesByZone zones [] -- [[ScoredTile]]
                              playersToBeScored = map (playersToScore) groupedZones -- [[Int]]
                              scoreAZone :: [ScoredTile] -> [Int] -> [ScoredTile]
                              scoreAZone [] _ = []
                              scoreAZone ((x,_):xs) pls = (x,score):(scoreAZone xs newpls)
                                    where shouldScore = elem ownerOfThisZone pls
                                          score = if shouldScore then scoreZone x b else 0
                                          newpls = filter (ownerOfThisZone/=) pls
                                          ownerOfThisZone = m_player.fromJust.t_meeple.getFirstTile $ x

-- | Agrupa os 'ScoredTile' caso se refiram à mesma 'Zone'
joinScoredTilesByZone :: [ScoredTile] -> [ScoredTile] -> [[ScoredTile]]
joinScoredTilesByZone [] _ = []
joinScoredTilesByZone (x:xs) feitos = mesmaZona : (joinScoredTilesByZone proximos feitosAgora)
                      where mesmaZona = x:(filter (\(y,_)->isSameZone (fst x) y) xs)
                            feitosAgora = feitos ++ mesmaZona
                            proximos = filter (\x-> not $ pertenceAfeitosAgora feitosAgora x) xs
                            pertenceAfeitosAgora [] _ = False
                            pertenceAfeitosAgora ((y,_):ys) (val,_) = if isSameZone y val then True else pertenceAfeitosAgora ys (val,0)

-- | Obtém uma lista com os jogadores que devem receber pontuação de acordo com o número de 'Meeple's que têm numa 'Zone'
playersToScore :: [ScoredTile] -> [Int]
playersToScore sTiles = map fst $ filter ((maxMeeples==).snd) playersWithMeeples
              where meeples = map (\(x,_)->fromJust.t_meeple.getFirstTile $ x) sTiles
                    allOwners = map (m_player) meeples
                    nrMeeples = map (length) (map (\x->filter (x==) allOwners) owners)
                    owners = removeDuplicates allOwners
                    playersWithMeeples = zipWith (\x y -> (x,y)) owners nrMeeples
                    maxMeeples = maximum nrMeeples

-- | Verifica se duas 'Zone's se referem à mesma região
isSameZone :: Zone -> Zone -> Bool
isSameZone (City x xs) (City y ys) = (tilesAreTheSame x y) || (tilesListsAreTheSame (xs) (ys))
isSameZone (Field x xs) (Field y ys) = (tilesAreTheSame x y) || (tilesListsAreTheSame (xs) (ys))
isSameZone (Cloister x xs) (Cloister y ys) = (tilesAreTheSame x y) || (tilesListsAreTheSame (xs) (ys))
isSameZone _ _ = False
