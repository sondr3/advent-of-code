module Y23Spec (spec) where

import Test.Hspec (Spec, parallel)
import TestUtils (testDay)
import Year.Y23.Day01 (day01)
import Year.Y23.Day02 (day02)
import Year.Y23.Day03 (day03)
import Year.Y23.Day04 (day04)
import Year.Y23.Day05 (day05)
import Year.Y23.Day06 (day06)
import Year.Y23.Day07 (day07)
import Year.Y23.Day08 (day08)
import Year.Y23.Day09 (day09)
import Year.Y23.Day10 (day10)
import Year.Y23.Day11 (day11)
import Year.Y23.Day12 (day12)

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
