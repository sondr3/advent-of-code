{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day02 where

import Data.Text (Text)
import Day (AoC, mkAoC)
import Parsers (Parser, lexeme)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Utils (pairwise, pairs)

type Input = [[Int]]

partA :: Input -> Int
partA xs = length . filter (== True) $ map (\x -> safe x && (inc x || dec x)) xs

safe :: (Eq a, Num a, Enum a) => [a] -> Bool
safe xs = any (== False) $ map (\(a, b) -> abs (a -b ) `elem` [1..3]) $ pairwise xs

inc :: Ord a => [a] -> Bool
inc xs = all (== True) $ map (\(a, b) -> (a < b)) $ pairwise xs

dec :: Ord a => [a] -> Bool
dec xs = all (== True) $ map (\(a, b) -> (a > b)) $ pairwise xs

partB :: Input -> Int
partB xs = undefined

parser :: Parser Input
parser = (some (some (lexeme L.decimal) <* optional eol)) <* eof

day02 :: AoC
day02 = mkAoC parser partA partB 2 2024
