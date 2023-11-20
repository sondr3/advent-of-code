module DaySpec (spec) where

import Test.Hspec (Spec, describe, it, parallel, shouldBe)

spec :: Spec
spec = parallel $ do
  describe "tests" $ do
    it "should pass" $ do
      1 `shouldBe` 1
