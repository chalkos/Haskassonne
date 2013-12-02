module Main where

import Text.XML.Light
import System.Random
import Tabuleiro
import Leitor
import Pontuar
import Escritor
import Text.Show.Pretty

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          seed <- randomRIO (0,1000)
          --putStrLn $ showElement (processa seed elem) --isto é o que é pedido?
          putStrLn $ ppElement (processa seed elem)
          --putStrLn $ ppShow (processa seed elem)

processa :: Int -> Element -> Element
processa seed e = board2element newBoard
      where b = processaBoard e
            newBoard = if isGameOver b then finalScore b else (substituteNext (updateScore b) (generateNext seed b))
