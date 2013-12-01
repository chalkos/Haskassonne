module Main where

import Text.XML.Light
import System.Random
import Tabuleiro
import Leitor
import Text.Show.Pretty

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          seed <- randomRIO (0,1000)
          --putStrLn $ showElement (processa seed elem)
          putStrLn $ ppShow (teste seed elem)

processa :: Int -> Element -> Element
processa seed e = if isGameOver board then finalScore b else (substituteNext (updateScore b) (randomValidTileToPlay seed b))
      where board = processaBoard e


-- TODO:
-- fazer a parte de calcular a pontuação