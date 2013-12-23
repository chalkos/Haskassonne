module Main where

import Text.XML.Light

import Leitor
import ArtASCII
import Tabuleiro

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          putStr $ processa elem

processa :: Element -> String
--processa e = ppShow (processaBoard e)
--processa e = ppShow (getLimits (processaBoard e))
{-processa e = ppShow (buildMap (b_terrain board) (getLimits board))
             where board = processaBoard e-}
--processa e = ppShow (rotateArt artTest 'N')
processa e = if hasNoTiles then "" else drawMap (buildMap (b_terrain board) (getLimits board))
            where board = processaBoard e
                  hasNoTiles = null $ b_terrain board