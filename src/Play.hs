module Main where

import Text.XML.Light
import System.Random
import Leitor
import Tabuleiro
import Escritor
import System.Environment

main = do entrada <- getContents
          args <- getArgs
          let Just elem = parseXMLDoc entrada
          seed <- randomRIO (0,1000)
          putStrLn $ (processa args seed elem)

processa :: [String] -> Int -> Element -> String
-- escreve o que é suposto para esta fase
processa [] seed e = tile2xmlString tileToPlay
           where board = (processaBoard e)
                 hasNoTiles = null $ b_terrain board
                 tileToPlay = if hasNoTiles then playFirstTile seed board else randomValidTileToPlay seed board
-- caso tenha argumentos escreve um xml completo com a jogada escolhida
processa _ seed e = ppElement $ if existeNext e then (board2element $ addTileToBoard board tileToPlay) else board2element.processaBoard $ e -- se a tag next nao existir, o jogo ja acabou e as pontuacoes ja foram calculadas, não deve fazer nada
           where board = (processaBoard e)
                 hasNoTiles = null $ b_terrain board
                 tileToPlay = if hasNoTiles then playFirstTile seed board else randomValidTileToPlay seed board





