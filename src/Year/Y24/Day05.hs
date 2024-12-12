{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day05 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Day (Day (..))
import Map (fromTuples)
import Parsers (Parser, number, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = (Map Int [Int], [[Int]])

partA :: Input -> Answer
partA (m, xs) = IntAnswer $ test (m, xs)

test :: (Map Int [Int], [[Int]]) -> Int
test (m, xs) = sumMiddle $ filterPages (m, xs) snd

find :: Map Int [Int] -> [Int] -> [Bool]
find _ [] = [True]
find _ [_] = [True]
find m (x : ys) = map (`elem` Map.findWithDefault [] x m) ys ++ find m ys

sumMiddle :: (Num a) => [[a]] -> a
sumMiddle xs = sum $ map (\x -> x !! (length x `div` 2)) xs

filterPages :: Input -> (([Int], Bool) -> Bool) -> [[Int]]
filterPages (m, xs) f = map fst $ filter f $ zip xs (map (and . find m) xs)

partB :: Input -> Answer
partB xs = IntAnswer . sumMiddle $ fixOrder xs

fixOrder :: Input -> [[Int]]
fixOrder (m, xs) = map go $ filterPages (m, xs) (not . snd)
  where
    go = until (and . find m) check
    check [] = []
    check [x] = [x]
    check (x : y : ds) = if x `elem` Map.findWithDefault [] y m then go (y : x : ds) else x : go (y : ds)

parser :: Parser Input
parser = (,) <$> pageOrder <* eol <*> (number `sepBy` symbol ",") `sepBy` eol <* eof
  where
    pageOrder :: Parser (Map Int [Int])
    pageOrder = fromTuples <$> some ((,) <$> (number <* symbol "|") <*> number <* eol)

day05 :: AoC Input
day05 = mkAoC parser partA partB D5 Y24
