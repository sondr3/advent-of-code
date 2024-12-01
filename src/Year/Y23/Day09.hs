{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y23.Day09 where

import Day (AoC, mkAoC)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char hiding (string)
import Utils (uTail)

partA :: [[Int]] -> Int
partA xs = sum $ map subseq xs

partB :: [[Int]] -> Int
partB xs = sum $ map (subseq . reverse) xs

subseq :: [Int] -> Int
subseq xs = sum $ map last $ takeWhile (any (/= 0)) $ iterate diff xs

diff :: (Num a) => [a] -> [a]
diff xs = zipWith (-) (uTail xs) xs

parser :: Parser [[Int]]
parser = (number `sepBy` hspace) `sepBy` eol

day09 :: AoC
day09 = mkAoC parser partA partB 9 2023
