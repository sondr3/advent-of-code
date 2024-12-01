{-# LANGUAGE FlexibleContexts #-}

module DaySpec (spec) where

import AoC (AoC (..), getDayDocument, mkAoC, whenJust)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Day.Day01 (day01)
import Day.Day02 (day02)
import Day.Day03 (day03)
import Day.Day04 (day04)
import Day.Day05 (day05)
import Day.Day06 (day06)
import Day.Day07 (day07)
import Day.Day08 (day08)
import Day.Day09 (day09)
import Day.Day10 (day10)
import Day.Day11 (day11)
import Day.Day12 (day12)
import Parsers (parseInput)
import TOML
import Test.Hspec (Spec, describe, it, parallel, runIO, shouldBe)
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
    whenJust (p1 $ answers i) $ \p -> part1 parsed `shouldBe` p
    whenJust (p2 $ answers i) $ \p -> part2 parsed `shouldBe` p

spec :: Spec
spec = parallel $ do
  testDay day01
  testDay day02
  testDay day03
  testDay day04
  testDay day05
  testDay day06
  testDay day07
  testDay day08
  testDay day09
  testDay day10
  testDay day11
  testDay day12
