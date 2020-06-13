module SymMat.Fraction (
  Fraction (..),
  fractionSimplify,
) where

import Data.List
import SymMat.Simplify
import SymMat.Factorisation

data Fraction = Fraction {
  numerator :: Int,
  denominator :: Int
} deriving (Eq)

instance Simplifiable Fraction where
  simplify x = fractionSimplify x
  toReal x = fracToReal x

instance Show Fraction where
  show (Fraction num den) = let
    chars = maximum [digits num,digits den]
    fracLine = take chars $ cycle "-"
    in intercalate "\n" [show num,fracLine,show den]

instance Num (Fraction) where
  Fraction a b + Fraction c d = Fraction (a*d + b*c) (b*d)
  Fraction a b * Fraction c d = Fraction (a*c) (b*d)
  Fraction a b - Fraction c d = Fraction (a*d - b*c) (b*d)
  abs (Fraction a b) = Fraction (abs a) (abs b)
  signum (Fraction a b) = Fraction (signum a) (signum b)
  fromInteger i = Fraction (fromInteger i) 1

digits :: (Num a,Show a) => a -> Int
digits x = length $ show x

fracToReal :: Fraction -> Float
fracToReal (Fraction num den) = (fromIntegral num) / (fromIntegral den)

-- | Simplifies a fraction
fractionSimplify :: Fraction -> Fraction
fractionSimplify (Fraction num 0) = Fraction num 0
fractionSimplify (Fraction num den) = let
  numF = (factorise num) ++ [1]
  denF = (factorise den) ++ [1]
  newNum = numF \\ denF
  newDen = denF \\ numF
  in Fraction (product newNum) (product newDen)

