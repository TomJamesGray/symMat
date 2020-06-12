module SymMat.FractionsSpec (spec) where

import SymMat.Fraction
import Test.QuickCheck
import Test.Hspec

-- | Tests simplification of fraction doesn't change real value
prop_simplifyEqual :: Int -> Int -> Bool
prop_simplifyEqual num 0 = True
prop_simplifyEqual num den = let
  valSimplified = toReal $ simplify $ Fraction num den
  initVal = toReal $ Fraction num den
  in valSimplified == initVal

spec :: Spec
spec = do
  describe "Fractions" $ do
    it "Simplified fraction is still equal to unsimplified fraction" $
      property prop_simplifyEqual