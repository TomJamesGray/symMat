module SymMat.FactorisationSpec (spec) where

import SymMat.Factorisation
import Test.QuickCheck
import Test.Hspec

prop_factorisation :: Int -> Bool
prop_factorisation n = foldl (*) 1 (factorise n) == n

spec :: Spec
spec = do
  describe "Factorisation" $ do
    it "Product of factors is equal to the number" $
      property prop_factorisation