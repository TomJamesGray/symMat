module SymMat.Fraction (
  Fraction (..),
  simplify,
  toReal
) where

import Data.List
import SymMat.Factorisation

data Fraction = Fraction {
  numerator :: Int,
  denominator :: Int
} deriving (Eq,Show)

toReal :: Fraction -> Float
toReal (Fraction num den) = (fromIntegral num) / (fromIntegral den)

simplify :: Fraction -> Fraction
simplify (Fraction num 0) = Fraction num 0
simplify (Fraction num den) = let
  numF = (factorise num) ++ [1]
  denF = (factorise den) ++ [1]
  newNum = numF \\ denF
  newDen = denF \\ numF
  in Fraction (product newNum) (product newDen)
