{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.DayXX where

import Data.Text (Text)
import Day (AoC, PartStatus (..), mkAoC)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Input = [Int]

partA :: Input -> PartStatus Int
partA xs = Unsolved

partB :: Input -> PartStatus Int
partB xs = Unsolved

parser :: Parser Input
parser = empty

day01 :: AoC
day01 = mkAoC parser partA partB 1 2024
