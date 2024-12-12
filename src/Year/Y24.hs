{-# LANGUAGE ExistentialQuantification #-}

module Year.Y24
  ( solutions,
  )
where

import Solutions (Solution (..))
import Year.Y24.Day01 (day01)
import Year.Y24.Day02 (day02)
import Year.Y24.Day03 (day03)

solutions :: [Solution]
solutions =
  [ Solution day01,
    Solution day02,
    Solution day03
  ]
