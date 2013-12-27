module Main where

import Text.XML.Light
import System.Random
import Tabuleiro
import Leitor
import Pontuar
import Escritor
import System.Environment
-- import FakePrettyShow
import FakePrettyShow

import Debug.Trace

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          seed <- randomRIO (0,1000)
          args <- getArgs
          putStrLn $ (if not $ null args then showElement else ppElement) (processa seed elem)

processa :: Int -> Element -> Element
processa seed e = board2element newBoard
      where b = processaBoard e
            newBoard = if isGameOver b then finalScore b else (substituteNext (updateScore b) (generateNext seed b))
