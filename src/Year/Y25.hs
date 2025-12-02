module Year.Y25
  ( solutions,
  )
where

import Day (Day (..))
import Solution (Solution (..))
import Year.Y25.Day01 (day01)
import Year.Y25.Day02 (day02)

solutions :: [(Day, Solution)]
solutions =
  [ (D1, Solution day01),
    (D2, Solution day02)
  ]
