module SymMat.SurdSpec (spec) where

import SymMat.Surd
import SymMat.Simplify
import Test.QuickCheck
import Test.Hspec

instance Arbitrary Surd where
  arbitrary = do
    Positive inner <- arbitrary
    scalar <- arbitrary
    return $ Surd inner scalar

marginErr :: Float
marginErr = 0.0001 -- Slightly more than margin of error for fractions

-- | Tests simplification of surd doesn't change real value
prop_simplifyEqual :: Surd -> Bool
prop_simplifyEqual x = let
  valSimplified = toReal $ simplify $ x
  initVal = toReal $ x
  in abs(valSimplified - initVal) < marginErr

spec :: Spec
spec = do
  describe "Surds" $ do
    it "Simplified surd is equal to unsimplified surd" $
      property prop_simplifyEqual