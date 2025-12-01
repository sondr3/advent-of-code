module Year.Y25
  ( solutions,
  )
where

import Day (Day (..))
import Solution (Solution (..))
import Year.Y25.Day01 (day01)

solutions :: [(Day, Solution)]
solutions =
  [ (D1, Solution day01)
  ]
