{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day11 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Data.MemoTrie (memo2)
import Day (Day (..))
import Parsers (Parser, number)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils (nDigits, splitNum, tupToList)

type Input = [Int]

partA :: Input -> Answer
partA = IntAnswer . sum . map (run 25)

partB :: Input -> Answer
partB = IntAnswer . sum . map (run 75)

run :: Int -> Int -> Int
run = memo2 go
  where
    go :: Int -> Int -> Int
    go 0 _ = 1
    go n num = sum $ fmap (run (n - 1)) (rules num)

rules :: Int -> [Int]
rules n
  | n == 0 = [1]
  | even (nDigits n) = half n
  | otherwise = [n * 2024]
  where
    half :: Int -> [Int]
    half = tupToList . splitNum

parser :: Parser Input
parser = number `sepBy` space <* eof

day11 :: AoC Input
day11 = mkAoC parser partA partB D11 Y24
