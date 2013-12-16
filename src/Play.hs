module Main where

import Text.XML.Light
import System.Random
import Leitor
import Tabuleiro
import Text.Show.Pretty
import Escritor
import System.Environment
import Debug.Trace

--main = do entrada <- getContents
--    let Just elem = parseXMLDoc entrada
--    putStrLn $ showElement (processa elem)
main = do entrada <- getContents
          args <- getArgs
          let Just elem = parseXMLDoc entrada
          seed <- randomRIO (0,1000)
          putStrLn $ (processa args seed elem)

processa :: [String] -> Int -> Element -> String
-- escreve o que é suposto para esta fase
processa [] seed e = tile2xmlString tileToPlay
           where 
              board = (processaBoard e)
              hasNoTiles = null $ b_terrain board
              tileToPlay = if hasNoTiles then playFirstTile seed board else randomValidTileToPlay seed board
-- caso tenha argumentos escreve um xml completo com a jogada escolhida
processa _ seed e = ppElement (board2element $ action)
           where 
              board = (processaBoard e)
              hasNoTiles = null $ b_terrain board
              hasReachedMaximunAllowedPlays = length (b_terrain board)
              tileToPlay = if hasNoTiles then playFirstTile seed board else randomValidTileToPlay seed board 
              action = if (hasReachedMaximunAllowedPlays<nPecasT) then addTileToBoard board tileToPlay
                       else board





