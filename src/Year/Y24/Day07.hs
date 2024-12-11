{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day07 where

import Day (AoC, mkAoC)
import Parsers (Parser, number, symbol)
import Puzzle.Types (Answer (..))
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = [(Int, [Int])]

partA :: Input -> Answer
partA xs = IntAnswer . foldl' (\acc ((r, _), b) -> acc + if b then r else 0) 0 $ zip xs $ map (uncurry go) xs
  where
    go :: Int -> [Int] -> Bool
    go res [x, y] = x * y == res || x + y == res
    go res (x : y : ys) = go res (x * y : ys) || go res (x + y : ys)
    go _ _ = False

partB :: Input -> Answer
partB xs = IntAnswer . foldl' (\acc ((r, _), b) -> acc + if b then r else 0) 0 $ zip xs $ map (uncurry go) xs
  where
    go :: Int -> [Int] -> Bool
    go res [x, y] = x * y == res || x + y == res || read (show x <> show y) == res
    go res (x : y : ys) = go res (x * y : ys) || go res (x + y : ys) || go res (read (show x <> show y) : ys)
    go _ _ = False

parser :: Parser Input
parser = ((,) <$> number <* symbol ":" <*> number `sepBy` hspace) `sepBy` eol

day07 :: AoC Input
day07 = mkAoC parser partA partB 7 2024
