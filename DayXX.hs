{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.DayXX where

import Data.Text (Text)
import Day (AoC, mkAoC)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Input = ([Any], [Any])

partA :: Input -> Int
partA xs = undefined

partB :: Input -> Int
partB xs = undefined

parser :: Parser Input
parser = undefined

day01 :: AoC
day01 = mkAoC parser partA partB 1 2024
