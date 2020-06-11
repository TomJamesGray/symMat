module SymMat.Fraction (
  Fraction (..)
) where

data Fraction = Fraction {
  numerator :: Int,
  denominator :: Int
} deriving (Eq,Show)