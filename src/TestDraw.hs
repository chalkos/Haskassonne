module Main where

import Test.QuickCheck

import Leitor
import ArtASCII
import Tabuleiro

import qualified Data.Set as Set

-- Properties must have monomorphic types. 
-- Polymorphic properties, must be restricted to a particular type to be used for testing. 
-- It is convenient to do so by stating the types of one or more arguments in a where types = (x1 :: t1, x2 :: t2, ...) clause.
-- Teste de idempotencia (Que tem a propriedade de poder ser aplicado mais do que uma vez sem que o resultado se altere)

prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]

-- | Declarar tipo IntWithLimitedRange para evitar declaracoes duplicadas (Int) ja definidas em Test.QuickCheck.Arbitrary
data IntWithLimitedRange = IntWithLimitedRange Int
--  deriving (Eq,Show)

instance Eq IntWithLimitedRange where  
    IntWithLimitedRange x == IntWithLimitedRange y = True
    IntWithLimitedRange _ /= IntWithLimitedRange _ = False 

instance Show IntWithLimitedRange where
  show (IntWithLimitedRange number) = show number

-- | Testa se dada uma orientação qualquer, a funcao rotateArt retorna uma peça orientada a norte
prop_rotateArt (x) = artAuxN == rotateArt artAuxN 'N' && 
                     artAuxE == rotateArt artAuxE 'N' &&
                     artAuxS == rotateArt artAuxS 'N' &&
                     artAuxW == rotateArt artAuxW 'N'
                     where artAuxN | x == (IntWithLimitedRange 1) = rotateArt artB 'N'
                                   | x == (IntWithLimitedRange 2) = rotateArt artC 'N'
                                   | x == (IntWithLimitedRange 3) = rotateArt artE 'N'
                                   | x == (IntWithLimitedRange 4) = rotateArt artN 'N'
                                   | x == (IntWithLimitedRange 5) = rotateArt artVoid 'N'
                           artAuxE | x == (IntWithLimitedRange 1) = rotateArt artB 'E'
                                   | x == (IntWithLimitedRange 2) = rotateArt artC 'E'
                                   | x == (IntWithLimitedRange 3) = rotateArt artE 'E'
                                   | x == (IntWithLimitedRange 4) = rotateArt artN 'E'
                                   | x == (IntWithLimitedRange 5) = rotateArt artVoid 'E'
                           artAuxS | x == (IntWithLimitedRange 1) = rotateArt artB 'S'
                                   | x == (IntWithLimitedRange 2) = rotateArt artC 'S'
                                   | x == (IntWithLimitedRange 3) = rotateArt artE 'S'
                                   | x == (IntWithLimitedRange 4) = rotateArt artN 'S'
                                   | x == (IntWithLimitedRange 5) = rotateArt artVoid 'S'
                           artAuxW | x == (IntWithLimitedRange 1) = rotateArt artB 'W'
                                   | x == (IntWithLimitedRange 2) = rotateArt artC 'W'
                                   | x == (IntWithLimitedRange 3) = rotateArt artE 'W'
                                   | x == (IntWithLimitedRange 4) = rotateArt artN 'W'
                                   | x == (IntWithLimitedRange 5) = rotateArt artVoid 'W'
                           types = x::IntWithLimitedRange

-- | Testa se o metodo removeDuplicates removes mesmo elementos duplicados
prop_removeDuplicates xs = dupBool (removeDuplicates xs)

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s 
                           then Just x
                           else dup' xs (Set.insert x s)

dupBool :: (Ord a, Show a) => [a] -> Bool
dupBool x = case dup x of Just x  -> False
                          Nothing -> True
