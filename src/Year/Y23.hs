module Year.Y23
  ( solutions,
  )
where

import Day (Day (..))
import Solution (Solution (..))
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

solutions :: [(Day, Solution)]
solutions =
  [ (D1, Solution day01),
    (D2, Solution day02),
    (D3, Solution day03),
    (D4, Solution day04),
    (D5, Solution day05),
    (D6, Solution day06),
    (D7, Solution day07),
    (D8, Solution day08),
    (D9, Solution day09),
    (D10, Solution day10),
    (D11, Solution day11),
    (D12, Solution day12),
    (D13, Solution day13),
    (D15, Solution day15)
  ]
