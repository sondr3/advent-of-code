{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day09 where

import Day (AoC, mkAoC)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char hiding (string)
import Universum
import Universum.Unsafe qualified as U

partA :: [[Int]] -> Int
partA xs = sum $ map subseq xs

partB :: [[Int]] -> Int
partB xs = sum $ map (subseq . reverse) xs

subseq :: [Int] -> Int
subseq xs = sum $ map U.last $ takeWhile (any (/= 0)) $ iterate diff xs

diff :: (Num a) => [a] -> [a]
diff xs = zipWith (-) (U.tail xs) xs

parser :: Parser [[Int]]
parser = (number `sepBy` hspace) `sepBy` eol

day09 :: AoC
day09 = mkAoC parser partA partB
