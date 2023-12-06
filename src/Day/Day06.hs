{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day06 where

import Day (AoC, mkAoC)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Universum
import Utils (intListToText)

partA :: [(Int, Int)] -> Int
partA xs = product $ map numWins xs

partB :: [(Int, Int)] -> Int
partB xs = numWins (intListToText (map fst xs), intListToText (map snd xs))

numWins :: (Int, Int) -> Int
numWins (t, d) = length [n | n <- [1 .. t], n * (t - n) > d]

parser :: Parser [(Int, Int)]
parser = zip <$> lineParser "Time:" <*> lineParser "Distance:"
  where
    lineParser :: Parser Text -> Parser [Int]
    lineParser t = lexeme t >> some (lexeme L.decimal) <* optional eol

day06 :: AoC
day06 = mkAoC parser partA partB
