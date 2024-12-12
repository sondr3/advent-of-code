{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y23.Day09 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Day (Day (..))
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char hiding (string)
import Utils (uTail)

partA :: [[Int]] -> Answer
partA xs = IntAnswer . sum $ map subseq xs

partB :: [[Int]] -> Answer
partB xs = IntAnswer . sum $ map (subseq . reverse) xs

subseq :: [Int] -> Int
subseq xs = sum $ map last $ takeWhile (any (/= 0)) $ iterate diff xs

diff :: (Num a) => [a] -> [a]
diff xs = zipWith (-) (uTail xs) xs

parser :: Parser [[Int]]
parser = (number `sepBy` hspace) `sepBy` eol

day09 :: AoC [[Int]]
day09 = mkAoC parser partA partB D9 Y23
