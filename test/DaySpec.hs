{-# LANGUAGE FlexibleContexts #-}

module DaySpec (spec) where

import AoC (AoC (..), getDayDocument, mkAoC)
import Day.Day01 (day01)
import Day.Day02 (day02)
import Day.Day03 (day03)
import Day.Day04 (day04)
import Day.Day05 (day05)
import Parsers (pLines)
import TOML
import Test.Hspec (Spec, describe, it, parallel, runIO, shouldBe)
import Universum
import Utils (padNum)

testDay :: Int -> AoC -> Spec
testDay day MkAoC {parse, part1, part2} = describe (toString $ "day " <> padNum day) $ do
  docs <- runIO (getDayDocument day)
  forM_ (inputs docs) $ \input -> do
    testInput input (mkAoC parse part1 part2)

testInput :: Input -> AoC -> Spec
testInput i MkAoC {parse, part1, part2} = do
  parsed <- pLines parse (input i)

  let name = fromMaybe "input" (comment i)
  it (toString $ "should parse " <> name) $ do
    whenJust (p1 $ answers i) $ \p -> part1 parsed `shouldBe` p
    whenJust (p2 $ answers i) $ \p -> part2 parsed `shouldBe` p

spec :: Spec
spec = parallel $ do
  testDay 1 day01
  testDay 2 day02
  testDay 3 day03
  testDay 4 day04
  testDay 5 day05
