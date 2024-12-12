module Year.Y23
  ( solutions,
  )
where

import Solutions (Solution (..))
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
import Year.Y23.Day13 (day13)
import Year.Y23.Day15 (day15)

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
    Solution day11,
    Solution day12,
    Solution day13,
    Solution day15
  ]
