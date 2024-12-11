{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y23.Day02 where

import Control.Monad (void)
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Day (AoC, mkAoC)
import Parsers (Parser)
import Puzzle.Types (Answer (..))
import Text.Megaparsec qualified as T
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Input = NonEmpty (NonEmpty (NonEmpty (Text, Int)))

partA :: Input -> Answer
partA xs = IntAnswer . sum $ zipWith (\i v -> if v then i else 0) ([1 ..] :: [Int]) $ NE.toList $ NE.map (all (all valid)) xs

valid :: (Text, Int) -> Bool
valid ("red", num) = num <= 12
valid ("green", num) = num <= 13
valid ("blue", num) = num <= 14
valid _ = error "invalid color"

partB :: Input -> Answer
partB xs = IntAnswer . sum $ NE.map go xs
  where
    go :: NonEmpty (NonEmpty (Text, Int)) -> Int
    go ys = Map.foldr (*) 1 $ Map.fromListWith max $ concatMap NE.toList ys

parser :: Parser (NonEmpty (NonEmpty (NonEmpty (Text, Int))))
parser = NE.some (void "Game " >> T.some digitChar >> ": " >> gameParser <* T.optional eol)
  where
    gameParser = bagParser `NE.sepBy1` "; "
    bagParser = colorParser `NE.sepBy1` ", "
    colorParser :: Parser (Text, Int)
    colorParser = L.decimal >>= \num -> void " " *> (T.pack <$> T.many lowerChar) >>= \color -> pure (color, num)

day02 :: AoC Input
day02 = mkAoC parser partA partB 2 2023
