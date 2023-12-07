{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day06 where

import Day (AoC, mkAoC)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Universum
import Utils (readConcat)

partA :: [(Int, Int)] -> Int
partA xs = product $ map numWins xs

partB :: [(Int, Int)] -> Int
partB xs = numWins (readConcat (map fst xs), readConcat (map snd xs))

numWins :: (Int, Int) -> Int
numWins (a, b) = abs (x1 - x2) + 1
  where
    t = fromIntegral a :: Double
    d = fromIntegral b
    delta = t * t - 4 * d
    x1 = ceiling $ (t + sqrt delta) / 2 - 1
    x2 = 1 + floor ((t - sqrt delta) / 2)

parser :: Parser [(Int, Int)]
parser = zip <$> lineParser "Time:" <*> lineParser "Distance:"
  where
    lineParser :: Parser Text -> Parser [Int]
    lineParser t = lexeme t >> some (lexeme L.decimal) <* optional eol

day06 :: AoC
day06 = mkAoC parser partA partB
