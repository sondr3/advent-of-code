module DaySpec (spec) where

import Test.Hspec (Spec, describe, it, parallel, shouldBe)
import Universum

spec :: Spec
spec = parallel $ do
  describe "tests" $ do
    it "should pass" $ do
      (1 :: Int) `shouldBe` 1
