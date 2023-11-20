module DaySpec (spec) where

import Test.Hspec (Spec, describe, it, parallel, runIO, shouldBe)
import Universum

spec :: Spec
spec = parallel $ do
  inputs <- runIO getInputs
  _ <- runIO $ traceShow ("Found " <> show (length inputs) <> " inputs") (putTextLn "")
  runIO $ putTextLn $ "Inputs: " <> show inputs
  describe "tests" $ do
    it "should pass" $ do
      (1 :: Int) `shouldBe` 1
