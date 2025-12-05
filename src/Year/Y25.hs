module Year.Y25
  ( solutions,
  )
where

import Solution (Solution (..))
import Year.Y25.Day01 (day01)
import Year.Y25.Day02 (day02)
import Year.Y25.Day03 (day03)
import Year.Y25.Day04 (day04)
import Year.Y25.Day05 (day05)

solutions :: [Solution]
solutions =
  [ Solution day01,
    Solution day02,
    Solution day03,
    Solution day04,
    Solution day05
  ]
