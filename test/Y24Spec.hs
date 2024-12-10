{-# LANGUAGE FlexibleContexts #-}

module Y24Spec (spec) where

import Test.Hspec (Spec, parallel)
import TestUtils (testDay)
import Year.Y24.Day01 (day01)
import Year.Y24.Day02 (day02)
import Year.Y24.Day03 (day03)
import Year.Y24.Day04 (day04)
import Year.Y24.Day05 (day05)
import Year.Y24.Day06 (day06)
import Year.Y24.Day07 (day07)
import Year.Y24.Day08 (day08)
import Year.Y24.Day09 (day09)
import Year.Y24.Day10 (day10)

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
