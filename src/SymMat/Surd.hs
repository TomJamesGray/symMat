module SymMat.Surd (
  Surd (..),
  surdSimplify,
  surdToReal
) where

import SymMat.Factorisation
import Data.List (nub)

data Surd = Surd {
  inner :: Int,
  scalar :: Int
} deriving (Eq,Show)

-- | Counts the number of occurences of an element in a list
numOccurences :: (Eq a) => a -> [a] -> Int
numOccurences x xs = length $ filter (x==) xs

-- | Computes a^x where they are both integers
intPow :: Int -> Int -> Int
intPow a x
  | x > 0 = foldl (*) 1 (take x (repeat a))

-- | Computes real value of surd
surdToReal :: Surd -> Float
surdToReal (Surd x sca) = fromIntegral sca * (fromIntegral x) ** 0.5

-- | Simplifies a surd
surdSimplify :: Surd -> Surd
surdSimplify (Surd x sca) = let
  xFactors = (factorise x) ++ [1]
  nubFactors = nub xFactors
  n = foldl (\acc fact -> let occ = numOccurences fact xFactors in
    case () of _
                | occ < 2 -> acc
                | (occ >= 2) && (occ `mod` 2 == 0) -> let
                                                       newIn = (inner acc) `div` intPow fact occ
                                                       newSca = (scalar acc) * intPow fact (occ `div` 2)
                                                       in Surd newIn newSca
                | (occ > 2) && (occ `mod` 2 == 1) -> let
                                                       newIn = (inner acc) `div` intPow fact (occ-1)
                                                       newSca = (scalar acc) * intPow fact ((occ-1) `div` 2)
                                                       in Surd newIn newSca
    ) (Surd x sca) nubFactors
  in n