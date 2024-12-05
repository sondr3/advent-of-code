{-# LANGUAGE FlexibleContexts #-}

module Y24Spec (spec) where

import Test.Hspec (Spec, parallel)
import TestUtils (testDay)
import Year.Y24.Day01 (day01)
import Year.Y24.Day02 (day02)
import Year.Y24.Day03 (day03)
import Year.Y24.Day04 (day04)
import Year.Y24.Day05 (day05)

spec :: Spec
spec = parallel $ do
  testDay day01
  testDay day02
  testDay day03
  testDay day04
  testDay day05
