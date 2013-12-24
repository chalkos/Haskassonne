module Main where

import Text.XML.Light
import System.Random
import Tabuleiro
import Leitor
import Pontuar
import Escritor
import Text.Show.Pretty
import Debug.Trace

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          seed <- randomRIO (0,1000)
          --putStrLn $ showElement (processa seed elem) -- isto é o que é pedido.
          putStrLn $ ppElement (processa seed elem)

processa :: Int -> Element -> Element
processa seed e = if existeNext e then board2element newBoard else board2element.processaBoard $ e -- se a tag next nao existir, o jogo ja acabou e as pontuacoes ja foram calculadas
      where b = processaBoard e
            newBoard = if isGameOver b then finalScore b else (substituteNext (updateScore b) (generateNext seed b))

-- TODO:
-- o isGameOver apenas dá True quando todas as peças foram jogadas.
--     isto não é correcto, pois se apenas faltar uma peça do tipo C (por exemplo) e não poder ser jogada, o jogo deve terminar
-- o finalScore não foi testado
