{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day01 where

import Data.List (sort)
import Day (AoC, mkAoC)
import Parsers (Parser, lexeme)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Input = ([Int], [Int])

partA :: Input -> Int
partA (xs, ys) = foldl' (\acc (x, y) -> acc + abs (x - y)) 0 (zip (sort xs) (sort ys))

partB :: Input -> Int
partB (xs, ys) = foldl' (\acc x -> acc + (x * length (filter (== x) ys))) 0 xs

parser :: Parser Input
parser = unzip <$> some ((,) <$> lexeme L.decimal <*> lexeme L.decimal <* optional eol) <* eof

day01 :: AoC
day01 = mkAoC parser partA partB 1 2024
