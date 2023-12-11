{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day04 where

import Data.Set qualified as S
import Day (AoC, mkAoC)
import Parsers (Parser)
import Text.Megaparsec hiding (some)
import Text.Megaparsec qualified as T
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Universum

partA :: [([Int], [Int])] -> Int
partA xs = sum . map (\x -> 2 ^ (x - 1)) . filter (> 0) $ map (uncurry numMatching) xs

numMatching :: [Int] -> [Int] -> Int
numMatching xs ys = S.size $ S.fromList xs `S.intersection` S.fromList ys

partB :: [([Int], [Int])] -> Int
partB i = sum $ go i
  where
    go :: [([Int], [Int])] -> [Int]
    go [] = []
    go ((xs, ys) : cs) =
      let cnt = go cs
          num = 1 + sum (take (numMatching xs ys) cnt)
       in num : cnt

parser :: Parser [([Int], [Int])]
parser = gameParser `sepEndBy` eol

gameParser :: Parser ([Int], [Int])
gameParser = do
  void $ "Card" >> hspace >> (L.decimal :: Parser Int) >> ": "
  winning <- hspace >> T.many (L.lexeme hspace L.decimal)
  void $ L.symbol hspace "|"
  yours <- T.many (L.lexeme hspace L.decimal)
  pure (winning, yours)

day04 :: AoC
day04 = mkAoC parser partA partB 4 2023
