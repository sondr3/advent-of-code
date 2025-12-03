{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y25.Day03 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Data.Char (digitToInt)
import Day (Day (..))
import Parsers (Parser, eolf)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string)
import Utils (uHead)

type Input = [[Int]]

partA :: Input -> Answer
partA xs = IntAnswer $ solve' 2 xs

partB :: Input -> Answer
partB xs = IntAnswer $ solve' 12 xs

solve' :: Int -> Input -> Int
solve' n xs = sum $ map (uHead . reverse . foldl' go (replicate n 0)) xs

parser :: Parser Input
parser = some (digitToInt <$> digitChar) `sepBy` eolf

go :: [Int] -> Int -> [Int]
go prev n = zipWith max prev (map (\b -> b * 10 + n) (0 : prev))

day03 :: AoC Input
day03 = mkAoC parser partA partB D3 Y25
