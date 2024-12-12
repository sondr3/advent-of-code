{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y23.Day05 where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Day (Answer (..), AoC, Year (..), mkAoC)
import GHC.Generics (Generic)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Utils (pairs)

data Range = Range Int Int Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

partA :: ([Int], [[Range]]) -> Answer
partA (seed, maps) = IntAnswer . minimum $ map (\x -> foldl go x maps) seed
  where
    go s [] = s
    go s (Range dst src len : xs)
      | src <= s && s < src + len = dst + s - src
      | otherwise = go s xs

concatRange :: NonEmpty (Int, Int) -> [Range] -> NonEmpty (Int, Int)
concatRange xs ys = NE.fromList $ concatMap (`foldRange` ys) xs

foldRange :: (Int, Int) -> [Range] -> [(Int, Int)]
foldRange t [] = [t]
foldRange t@(s, len) (Range dst src len' : rs)
  | s <= src + len' && src < s + len = concat [pre, curr, post]
  | otherwise = foldRange t rs
  where
    pre = if s < src then foldRange (s, src - s) rs else []
    curr = [(dst + max 0 (s - src), min len (len' - max 0 (s - src)))]
    post = if src + len' < s + len then foldRange (src + len', s + len - src - len') rs else []

partB :: ([Int], [[Range]]) -> Answer
partB (seeds, maps) = IntAnswer . fst $ minimum $ foldl concatRange (NE.fromList $ pairs seeds) maps

parser :: Parser ([Int], [[Range]])
parser = (,) <$> parseSeeds <*> parseMap `sepEndBy` "\n"

parseSeeds :: Parser [Int]
parseSeeds = lexeme "seeds:" *> some (lexeme L.decimal)

parseRange :: Parser Range
parseRange = Range <$> lexeme L.decimal <*> lexeme L.decimal <*> lexeme L.decimal

parseMap :: Parser [Range]
parseMap = takeWhileP Nothing (/= ':') >> char ':' >> eol >> parseRange `sepEndBy1` "\n"

day05 :: AoC ([Int], [[Range]])
day05 = mkAoC parser partA partB 5 Y23
