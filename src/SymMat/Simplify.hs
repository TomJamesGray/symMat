module SymMat.Simplify (
  Simplifiable (..)
) where

class Simplifiable a where
  simplify :: a -> a
  toReal :: a -> Float
