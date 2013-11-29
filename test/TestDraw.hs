module Main where

import Text.XML.Light
import Test.QuickCheck
import Data.List.Utils
import System.IO

import Leitor
import ArtASCII

main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
          putStrLn $ (processa elem)
          
          --contents <- readFile "test/001_test_result.xml"
          --print . map readString . words $ contents
          --alternately:
          --print . map readString . words =<< readFile "test/001_test_result.xml"
          --print =<< readFile "test/001_test_result.xml"
          

processa :: Element -> String
--processa e = ppShow (processaBoard e)
--processa e = ppShow (getLimits (processaBoard e))
{-processa e = ppShow (buildMap (b_terrain board) (getLimits board))
             where board = processaBoard e-}
--processa e = ppShow (rotateArt artTest 'N')
processa e = drawMap (buildMap (b_terrain board) (getLimits board))
             where board = processaBoard e

-- Testing related
-- Properties must have monomorphic types. 
-- Polymorphic properties, must be restricted to a particular type to be used for testing. 
-- It is convenient to do so by stating the types of one or more arguments in a where types = (x1 :: t1, x2 :: t2, ...) clause.
prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]

--http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
--http://stackoverflow.com/questions/16201741/using-quickcheck
--http://stackoverflow.com/questions/5208621/show-ing-functions-used-in-quickcheck-properties

data IntWithLimitedRange = IntWithLimitedRange Int

instance Show IntWithLimitedRange where
  show (IntWithLimitedRange number) = show number

instance Arbitrary IntWithLimitedRange where
  arbitrary = do
    number <- choose (1, 2)
    return $ IntWithLimitedRange number

prop_resultado_1 (xs) = boolFromIO (fazComparacao xs)
                      where types = xs::IntWithLimitedRange

boolFromIO :: IO Bool -> Bool
boolFromIO = boolFromIO

fazComparacao :: IntWithLimitedRange -> IO Bool 
fazComparacao xs = do input <- readFile (nomeFicheiroTestesParaPathCorrecto xs)
                      resultadoEsperado <- readFile (nomeFicheiroTestesResultadoParaPathCorrecto xs)
                      let Just elem = parseXMLDoc input
                      return (resultadoEsperado == (processa elem))

nomeFicheiroTestesParaPathCorrecto :: IntWithLimitedRange -> String
nomeFicheiroTestesParaPathCorrecto s = "test/" ++ show s ++ ".xml"

nomeFicheiroTestesResultadoParaPathCorrecto :: IntWithLimitedRange -> String
nomeFicheiroTestesResultadoParaPathCorrecto s = "test/" ++ show s ++ "_test_result.xml"







