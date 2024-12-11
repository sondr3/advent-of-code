{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day10 where

import Coordinates (Position, cardinals, neighbours)
import Data.Char (digitToInt)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Display (display)
import Day (AoC, mkAoC)
import Grid (Grid, gridify, onGrid)
import Parsers (Parser, symbol)
import Puzzle.Types (Answer (..))
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, eol)

data Trail
  = Path Int
  | Impassable
  deriving stock (Show, Eq, Ord)

type Location = (Position, Trail)

type Input = Grid Trail

findStart :: Input -> [(Position, Trail)]
findStart = Map.toList . Map.filter (== Path 0)

canMove :: Trail -> Maybe Trail -> Bool
canMove (Path start) (Just (Path end)) = start + 1 == end
canMove _ _ = False

validMoves :: (Position, Trail) -> Input -> [(Position, Trail)]
validMoves (pos, p@(Path _)) grid = map (\x -> (x, (Map.!) grid x)) $ filter (\x -> onGrid grid x && canMove p (Map.lookup x grid)) $ neighbours pos cardinals
validMoves _ _ = []

walk :: Location -> Input -> [[Location]]
walk cur grid = case validMoves cur grid of
  [] -> [[cur]]
  moves -> map (cur :) $ concatMap (`walk` grid) moves

findAllPaths :: Input -> [[[Location]]]
findAllPaths grid = map (filter (\x -> length x == 10) . (`walk` grid)) (findStart grid)

partA :: Input -> Answer
partA grid = IntAnswer . sum . map (length . Set.fromList . map last) $ findAllPaths grid

partB :: Input -> Answer
partB grid = IntAnswer . sum . map length $ findAllPaths grid

parser :: Parser Input
parser = gridify <$> (some . choice) [Path . digitToInt <$> digitChar, Impassable <$ symbol "."] `sepBy` eol <* eof

pretty :: Trail -> Text
pretty Impassable = "."
pretty (Path n) = display n

day10 :: AoC Input
day10 = mkAoC parser partA partB 10 2024
