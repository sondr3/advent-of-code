{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day01 where

import Day (AoC, mkAoC)
import Parsers (Parser)
import Universum

partA :: [[Int]] -> Int
partA _ = 0

partB :: [[Int]] -> Int
partB _ = 1

parser :: Parser [[Int]]
parser = pure []

day01 :: AoC
day01 = mkAoC parser partA partB (pure [0, 1])
