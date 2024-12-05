{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day05 where

import Control.Arrow ((&&&))
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Day (AoC, PartStatus (..), mkAoC)
import Map (fromTuples)
import Parsers (Parser, number, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = (Map Int [Int], [[Int]])

partA :: Input -> PartStatus
partA (m, xs) = Solved $ test (m, xs)

test :: (Map Int [Int], [[Int]]) -> Int
test (m, xs) = sum $ map (\x -> x !! (length x `div` 2)) $ map (\(x, _) -> x) $ filter snd $ zip xs (map (all id) $ map (find m) xs)

find :: Map Int [Int] -> [Int] -> [Bool]
find _ [] = [True]
find _ [_] = [True]
find m (x : ys) = do
  let pages = Map.findWithDefault [] x m
  map (\p -> p `elem` pages) ys ++ find m ys

partB :: Input -> PartStatus
partB xs = Unsolved

parser :: Parser Input
parser = (,) <$> pageOrder <* eol <*> (number `sepBy` symbol ",") `sepBy` eol <* eof
  where
    pageOrder :: Parser (Map Int [Int])
    pageOrder = fromTuples <$> some ((,) <$> (number <* symbol "|") <*> number <* eol)

day05 :: AoC Input
day05 = mkAoC parser partA partB 5 2024
