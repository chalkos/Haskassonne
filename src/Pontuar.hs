module Pontuar where

import Text.XML.Light
import Tabuleiro
import Leitor

type ScoredTile = (Tile, Int)

-- | Calcula as pontuações, inclusive dos Farmers, retira todos os meeples e atribui pontuações finais
finalScore :: Board -> Board
finalScore b = b

-- | Retira os meeples e atribui pontuações
updateScore :: Board -> Board
updateScore b = b

-- | Obtém a lista de 'Tile's com 'Meeple's que devem ser removidos
obterMeeplesPorRetirar :: Board -> [ScoredTile]
obterMeeplesPorRetirar b = monks ++ knights
                      where tilesWithMeeples = getAllTilesWithMeeples b
                            monks = map (getScoredTileFromTile) (filter (isMonkDone b) tilesWithMeeples)
                            knights = map (getScoredTileFromTile) (filter (isKnightDone b) tilesWithMeeples)

-- | Obtém um 'ScoredTile' a partir de um 'Tile'. A pontuação inicial é 0.
getScoredTileFromTile :: Tile -> ScoredTile
getScoredTileFromTile t = (t,0)

-- | Verifica se o monk deve ser retirado do tabuleiro
isMonkDone :: Board -> Tile -> Bool
isMonkDone b tMeeple = (m_type meeple) == 'M' && existsTile (x-1,y+1) && existsTile (x,y+1) && existsTile (x+1,y+1) && existsTile (x-1,y) && existsTile (x+1,y) && existsTile (x-1,y-1) && existsTile (x,y-1) && existsTile (x+1,y-1)
          where meeple = fromJust.t_meeple $ tMeeple
                tiles = b_terrain b
                x = t_x tMeeple
                y = t_y tMeeple
                existsTile location = (isJust.getTileAtLocation) tiles location

-- | Verifica se o knight deve ser retirado do tabuleiro
isKnightDone :: Board -> Tile -> Bool
isKnightDone b tMeeple = (m_type meeple) == 'K' && isCityComplete (x,y) tiles
            where meeple = fromJust.t_meeple $ tMeeple
                  tiles = b_terrain b
                  x = t_x tMeeple
                  y = t_y tMeeple






-- para cada meeple calcular quantos pontos vale
-- retirar os meeple que já foram pontuados