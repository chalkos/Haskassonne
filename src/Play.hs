module Main where

import Text.XML.Light
import System.Random
import Leitor
import Tabuleiro
import Text.Show.Pretty
import Escritor

--main = do entrada <- getContents
--    let Just elem = parseXMLDoc entrada
--    putStrLn $ showElement (processa elem)
main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          seed <- randomRIO (0,1000)
          putStrLn $ (processa seed elem)

processa :: Int -> Element -> String
--processa e = ppShow (possibleBordersOfTileAt 1 (-1) tiles)
--processa e = ppShow (possibleTilesAt (-2,0) tiles)
--processa e = ppShow (possibleNextTiles board)
--processa e = ppShow (validNextTiles board)
--processa e = ppShow (randomValidNextTile board)
processa seed e = tile2xmlString (randomValidTileToPlay seed board)
         where 
            --(tiles, players, proxima) = (b_terrain board, b_scores board, b_next board)
            board = (processaBoard e)