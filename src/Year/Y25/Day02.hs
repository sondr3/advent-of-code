{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y25.Day02 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Data.Text (Text)
import Data.Text qualified as T
import Day (Day (..))
import Parsers (Parser, lexeme, string, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string)
import Text.Megaparsec.Char.Lexer qualified as L
import Utils (chunksOf, intToText, read', textToInt)

type Input = [(Int, Int)]

partA :: Input -> Answer
partA xs = IntAnswer $ sum $ concatMap ((map (read . fst) . filter (\(_, (a, b)) -> a == b)) . map (\t -> (t, splitAt (half t) t))) (conv xs)

partB :: Input -> Answer
partB xs = NilAnswer

parser :: Parser Input
parser = liftA2 (,) (lexeme L.decimal <* symbol "-") (lexeme L.decimal) `sepBy` symbol ","

-- test :: Input -> Int
-- test xs = map (\str -> map (\t -> chunksOf t str) (chunks str)) (conv xs)
test :: Input -> [[(String, [[String]])]]
test xs = map (map (\(st, ix) -> (st, map (\i -> chunksOf i st) ix))) $ map (map (\s -> (s, chunks s))) $ conv xs

test2 xs = concatMap (map fst . filter (\(_, ix) -> any (== True) ix)) $ map (map (\(st, ix) -> (st, map (\xs -> all (== (head xs)) (tail xs)) ix))) $ test xs

conv :: Input -> [[String]]
conv = map (map show . uncurry enumFromTo)

eq :: [String] -> Bool
eq [a, b] = a == b
eq (a : b : _) = a == b
eq _ = False

half :: String -> Int
half t = ceiling $ fromIntegral (length t) / 2

chunks :: String -> [Int]
chunks t = enumFromTo 1 (half t)

day02 :: AoC Input
day02 = mkAoC parser partA partB D2 Y25
