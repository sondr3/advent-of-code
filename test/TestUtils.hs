{-# LANGUAGE FlexibleContexts #-}

module TestUtils (testDay, testInput) where

import AoC (AoC (..), getDayDocument, mkAoC)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Parsers (parseInput)
import TOML
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Utils (padNum)

testDay :: AoC -> Spec
testDay MkAoC {parse, part1, part2, day, year} = describe (T.unpack $ "day " <> padNum day) $ do
  docs <- runIO (getDayDocument day year)
  forM_ (inputs docs) $ \input -> do
    testInput input (mkAoC parse part1 part2 day year)

testInput :: Input -> AoC -> Spec
testInput i MkAoC {parse, part1, part2} = do
  parsed <- parseInput parse (comment i) (input i)

  let name = fromMaybe "input" (comment i)
  it (T.unpack $ "should parse " <> name) $ do
    whenAnswer (p1 $ answers i) $ \p -> part1 parsed `shouldBe` p
    whenAnswer (p2 $ answers i) $ \p -> part2 parsed `shouldBe` p
