{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day02 where

import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Map qualified as Map
import Day (AoC, mkAoC)
import Parsers (Parser)
import Text.Megaparsec qualified as T
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Universum

type Input = NonEmpty (NonEmpty (NonEmpty (Text, Int)))

partA :: Input -> Int
partA xs = sum $ zipWith (\i v -> if v then i else 0) [1 ..] $ toList $ map (all (all valid)) xs

valid :: (Text, Int) -> Bool
valid ("red", num) = num <= 12
valid ("green", num) = num <= 13
valid ("blue", num) = num <= 14
valid _ = error "invalid color"

partB :: Input -> Int
partB xs = sum $ map go xs
  where
    go :: NonEmpty (NonEmpty (Text, Int)) -> Int
    go ys = Map.foldr (*) 1 $ Map.fromListWith max $ concatMap toList ys

parser :: Parser (NonEmpty (NonEmpty (NonEmpty (Text, Int))))
parser = NE.some (void "Game " >> T.some digitChar >> ": " >> gameParser <* eol)
  where
    gameParser = bagParser `NE.sepBy1` "; "
    bagParser = colorParser `NE.sepBy1` ", "
    colorParser :: Parser (Text, Int)
    colorParser = L.decimal >>= \num -> void " " *> (toText <$> T.many lowerChar) >>= \color -> pure (color, num)

day02 :: AoC
day02 = mkAoC parser partA partB (pure [8, 2278, 2286, 67953])
