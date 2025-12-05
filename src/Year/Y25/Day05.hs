{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y25.Day05 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Data.List (sortOn)
import Day (Day (..))
import Parsers (Parser, eolf, number, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = ([(Int, Int)], [Int])

partA :: Input -> Answer
partA (rs, ns) = IntAnswer . length . filter id $ map (or . (\n -> map (`numBetween` n) rs)) ns

partB :: Input -> Answer
partB (rs, _) = IntAnswer . sum . map (\(a, b) -> b - a + 1) $ mergeOverlap (sortOn fst rs)

parser :: Parser Input
parser = (,) <$> some ((,) <$> (number <* symbol "-") <*> number <* eol) <* eol <*> number `sepBy` eolf

numBetween :: (Int, Int) -> Int -> Bool
numBetween (min', max') s = s >= min' && s <= max'

inside :: (Int, Int) -> (Int, Int) -> Bool
inside (_, x2) (y1, _) = x2 >= y1

mergeOverlap :: [(Int, Int)] -> [(Int, Int)]
mergeOverlap [] = []
mergeOverlap [x] = [x]
mergeOverlap (a@(x1, x2) : b@(_, y2) : xs)
  | inside a b = mergeOverlap ((x1, max x2 y2) : xs)
  | otherwise = a : mergeOverlap (b : xs)

day05 :: AoC Input
day05 = mkAoC parser partA partB D5 Y25
