module Main where

import Text.XML.Light
import Leitor
import Tabuleiro
import Text.Show.Pretty

--main = do entrada <- getContents
--    let Just elem = parseXMLDoc entrada
--    putStrLn $ showElement (processa elem)
main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          putStrLn $ (processa elem)

processa :: Element -> String
--processa e = ppShow (possibleBordersOfTileAt 1 (-1) tiles)
--processa e = ppShow (possibleTilesAt (-2,0) tiles)
processa e = ppShow (possibleNextTiles board)
             where 
                --(tiles, players, proxima) = (b_terrain board, b_scores board, b_next board)
                board = (processaBoard e)


-- verificar se se pode por lá um meeple
    -- verificar recursivamente todas as peças à volta
        -- se encontrar outro meeple na mesma zona, não posso por

-- obtendo uma lista de peças que podem ser colocadas, em que posições e com que meeples,
--   basta escolher um desses Tile de forma aletória e jogar esse
