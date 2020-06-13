module SymMat.FractionsSpec (spec) where

import SymMat.Fraction
import SymMat.Simplify
import Test.QuickCheck
import Test.Hspec

instance Arbitrary Fraction where
  arbitrary = do
    NonZero num <- arbitrary
    NonZero den <- arbitrary
    return $ Fraction num den

marginErr :: Float
marginErr = 0.00001

-- | Tests simplification of fraction doesn't change real value
prop_simplifyEqual :: Int -> Int -> Bool
prop_simplifyEqual num 0 = True
prop_simplifyEqual num den = let
  valSimplified = toReal $ simplify $ Fraction num den
  initVal = toReal $ Fraction num den
  in valSimplified == initVal

-- | Tests addition of fractions
prop_addition :: Fraction -> Fraction -> Bool
prop_addition fracA fracB = let
  eval1 = toReal (fracA + fracB)
  eval2 = (toReal fracA) + (toReal fracB)
  in abs(eval1 - eval2) < marginErr -- Can't test for equality due to floating point errors

spec :: Spec
spec = do
  describe "Fractions" $ do
    it "Simplified fraction is still equal to unsimplified fraction" $
      property prop_simplifyEqual

    it "Adding fractions works as expected" $
      property prop_addition