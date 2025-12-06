module Y25Spec (spec) where

import Test.Hspec (Spec, parallel)
import TestUtils (testDay)
import Year.Y25.Day01 (day01)
import Year.Y25.Day02 (day02)
import Year.Y25.Day03 (day03)
import Year.Y25.Day04 (day04)
import Year.Y25.Day05 (day05)
import Year.Y25.Day06 (day06)

spec :: Spec
spec = parallel $ do
  testDay day01
  testDay day02
  testDay day03
  testDay day04
  testDay day05
  testDay day06
