{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y25.Day06 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Data.List (transpose)
import Data.List.Split (splitWhen)
import Data.Text qualified as T
import Day (Day (..))
import Parsers (Parser)
import Text.Megaparsec
import Utils (rotate, uHead, uTail)

type Input = [String]

partA :: Input -> Answer
partA xs = IntAnswer $ sum $ map (go . rotate) (transpose $ map words xs)

partB :: Input -> Answer
partB xs = IntAnswer $ sum $ map go $ zipWith (:) ops ns
  where
    ys = rotate xs
    ns = splitWhen (all (== ' ')) . transpose $ uTail ys
    ops = words (uHead ys)

go :: [String] -> Int
go ("+" : xs) = sum $ map read xs
go ("*" : xs) = product $ map read xs
go _ = error "type check"

parser :: Parser Input
parser = lines . T.unpack <$> getInput

day06 :: AoC Input
day06 = mkAoC parser partA partB D6 Y25
