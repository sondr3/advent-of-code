{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day13 where

import Day (AoC, mkAoC)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char hiding (string)
import Universum
import Universum.Unsafe qualified as U
import Utils (listToMaybe)

data Pattern = Ash | Rock deriving stock (Show, Eq, Ord, Generic)

partA :: [[[Pattern]]] -> Int
partA xs = sum $ map calculate xs

partB :: [[[Pattern]]] -> Int
partB xs = 0

calculate :: (Eq a) => [[a]] -> Int
calculate grid = fromMaybe 0 $ ((* 100) <$> pivot grid) <|> pivot (transpose grid)

pivot :: (Eq a) => [a] -> Maybe Int
pivot grid = length . fst <$> find (\(xs, ys) -> and $ mirrored xs ys) (filter (\(xs, ys) -> not (null xs) && not (null ys)) $ zip (inits grid) (tails grid))
  where
    mirrored :: (Eq b) => [b] -> [b] -> [Bool]
    mirrored xs = zipWith (==) (reverse xs)

parser :: Parser [[[Pattern]]]
parser = ((some . choice) [Ash <$ char '.', Rock <$ char '#'] `sepEndBy` eol) `sepEndBy` some eol

day13 :: AoC
day13 = mkAoC parser partA partB 13 2023
