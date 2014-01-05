module Main where

import Text.XML.Light
import System.Random
import Tabuleiro
import Leitor
import Pontuar
import Escritor
import System.Environment

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          seed <- randomRIO (0,1000)
          args <- getArgs
          putStrLn $ (if null args then showElement else ppElement) (processa seed elem)

processa :: Int -> Element -> Element
processa seed e = board2element newBoard
-- processa seed e = board2element (finalScore b)
      where b = processaBoard e
            newBoard | null.b_terrain $ b = substituteNext b (Next 'E')
                     | isGameOver b = finalScore b
                     | otherwise = substituteNext (updateScore b) (generateNext seed b)
