{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y23.Day09 where

import Day (AoC, PartStatus (..), mkAoC)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char hiding (string)
import Utils (uTail)

partB :: [[Int]] -> PartStatus

partA xs = Solved . sum $ map subseq xs

partA :: [[Int]] -> PartStatus

partB xs = Solved . sum $ map (subseq . reverse) xs

subseq :: [Int] -> Int
subseq xs = sum $ map last $ takeWhile (any (/= 0)) $ iterate diff xs

diff :: (Num a) => [a] -> [a]
diff xs = zipWith (-) (uTail xs) xs

parser :: Parser [[Int]]
parser = (number `sepBy` hspace) `sepBy` eol

day09 :: AoC
day09 = mkAoC parser partA partB 9 2023
