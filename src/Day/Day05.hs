{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day05 where

import Day (AoC, mkAoC)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Universum
import Utils (pairs)

data Range = Range Int Int Int
  deriving stock (Eq, Show)

partA :: ([Int], [[Range]]) -> Int
partA (seed, maps) = minimum $ map (\x -> foldl go x maps) (fromList seed)
  where
    go s [] = s
    go s (Range dst src len : xs)
      | src <= s && s < src + len = dst + s - src
      | otherwise = go s xs

concatRange :: NonEmpty (Int, Int) -> [Range] -> NonEmpty (Int, Int)
concatRange xs ys = fromList $ concatMap (`foldRange` ys) xs

foldRange :: (Int, Int) -> [Range] -> [(Int, Int)]
foldRange t [] = [t]
foldRange t@(s, len) (Range dst src len' : rs)
  | s <= src + len' && src < s + len = concat [pre, curr, post]
  | otherwise = foldRange t rs
  where
    pre = if s < src then foldRange (s, src - s) rs else []
    curr = [(dst + max 0 (s - src), min len (len' - max 0 (s - src)))]
    post = if src + len' < s + len then foldRange (src + len', s + len - src - len') rs else []

partB :: ([Int], [[Range]]) -> Int
partB (seeds, maps) = fst $ minimum $ foldl concatRange (fromList $ pairs seeds) maps

parser :: Parser ([Int], [[Range]])
parser = (,) <$> parseSeeds <*> parseMap `sepEndBy` "\n"

parseSeeds :: Parser [Int]
parseSeeds = lexeme "seeds:" *> some (lexeme L.decimal)

parseRange :: Parser Range
parseRange = Range <$> lexeme L.decimal <*> lexeme L.decimal <*> lexeme L.decimal

parseMap :: Parser [Range]
parseMap = takeWhileP Nothing (/= ':') >> char ':' >> eol >> parseRange `sepEndBy1` "\n"

day05 :: AoC
day05 = mkAoC parser partA partB 5 2023
