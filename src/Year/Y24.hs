module Year.Y24
  ( solutions,
  )
where

import Solution (Solution (..))
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
import Year.Y24.Day11 (day11)

solutions :: [Solution]
solutions =
  [ Solution day01,
    Solution day02,
    Solution day03,
    Solution day04,
    Solution day05,
    Solution day06,
    Solution day07,
    Solution day08,
    Solution day09,
    Solution day10,
    Solution day11
  ]
