-- | Trata de contar os pontos
module Pontuar where

import Text.XML.Light
import Tabuleiro
import Leitor
import Data.Maybe
import Data.List


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
                  realScores = getRealScoresForZones $ map (\x->(getZoneForMeeple b x,0)) (getAllTilesWithMeeples b)
                  scoredPlayers = scorePlayers realScores players
                  tilesWithoutMeeples = removeMeeples realScores (b_terrain b)
-- map (getZoneForMeeple b) (getAllTilesWithMeeples b) -- Obtém 'ScoredTile's que devem ser pontuados

-- | Retira os meeples e atribui pontuações
updateScore :: Board -> Board
updateScore b = (Board {b_terrain=tilesWithoutMeeples, b_scores=scoredPlayers, b_next=(b_next b)})
            where players = b_scores b
                  realScores = getRealScoresForZones $ obterMeeplesPorRetirar b
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
scoreZone :: Zone -> Int
scoreZone (Cloister _ tiles) = (length tiles)-1
scoreZone (City _ tiles) = length tiles
scoreZone (Field _ tiles) = length tiles

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
getRealScoresForZones :: [ScoredTile] -> [ScoredTile]
getRealScoresForZones zones = concat $ zipWith (scoreAZone) groupedZones playersToBeScored
                      where groupedZones = joinScoredTilesByZone zones [] -- [[ScoredTile]]
                            playersToBeScored = map (playersToScore) groupedZones -- [[Int]]
                            scoreAZone :: [ScoredTile] -> [Int] -> [ScoredTile]
                            scoreAZone [] _ = []
                            scoreAZone ((x,_):xs) pls = (x,score):(scoreAZone xs pls)
                                  where shouldScore = elem ((m_player.fromJust.t_meeple.getFirstTile) x) pls
                                        score = if shouldScore then scoreZone x else 0

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


-- para cada meeple calcular quantos pontos vale
-- retirar os meeple que já foram pontuados