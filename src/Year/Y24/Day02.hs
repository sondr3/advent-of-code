{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day02 where

import Day (AoC, mkAoC)
import Parsers (Parser, lexeme)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Utils (dropped, pairwise)

type Input = [[Int]]

partA :: Input -> Int
partA = length . run

run :: Input -> [Bool]
run xs = filter id $ map (\x -> safe x && (ordered x (<) || ordered x (>))) xs

safe :: (Eq a, Num a, Enum a) => [a] -> Bool
safe xs = all (\(a, b) -> abs (a - b) `elem` [1 .. 3]) (pairwise xs)

ordered :: [a] -> (a -> a -> Bool) -> Bool
ordered xs op = all (uncurry op) (pairwise xs)

partB :: Input -> Int
partB xs = length $ filter (not . null) $ map (run . dropped) xs

parser :: Parser Input
parser = some (some (lexeme L.decimal) <* optional eol) <* eof

day02 :: AoC
day02 = mkAoC parser partA partB 2 2024
