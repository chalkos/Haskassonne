module Main where

import Text.XML.Light
import Data.List.Utils

import Leitor
import ArtASCII

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          putStrLn $ (processa elem)

processa :: Element -> String
--processa e = ppShow (processaBoard e)
--processa e = ppShow (getLimits (processaBoard e))
{-processa e = ppShow (buildMap (b_terrain board) (getLimits board))
             where board = processaBoard e-}
--processa e = ppShow (rotateArt artTest 'N')
processa e = drawMap (buildMap (b_terrain board) (getLimits board))
             where board = processaBoard e