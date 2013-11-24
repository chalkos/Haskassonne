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
processa seed e = e

teste :: Int -> Element -> (Int,Int,Int,Int)
teste seed e = pecasRestantes board
      where board = processaBoard e

-- TODO:
-- verificar quantas peças ainda podem ser jogadas, se for 0, terminar o jogo (módulo Pontuar)
-- verificar que peças podem ser colocadas e onde e com que meeples (módulo Tabuleiro)
-- verificar quantas peças ainda podem ser jogadas de cada tipo e remover os Tiles que não podem ser escolhidos para a próxima jogada
-- escolher uma das peças possíveis