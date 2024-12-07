{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day06 where

import Coordinates (Dir (East, North, South, West), Position, Turn (..), move, turnCardinal)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Day (AoC, PartStatus (..), mkAoC)
import Grid (findSingle, gridify)
import Parsers (Parser, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char

data Point
  = Obstacle
  | Open
  | Player Dir
  deriving stock (Show, Eq)

type Input = Map Position Point

partA :: Input -> PartStatus Int
partA xs = Solved . Set.size . Set.fromList . map fst $ path xs (findSingle xs (Player North), North)

path :: Input -> (Position, Dir) -> [(Position, Dir)]
path xs (pos, dir) =
  (pos, dir) : case Map.lookup (move pos dir) xs of
    Just Obstacle -> path xs (pos, turnCardinal dir TRight)
    Just _ -> path xs (move pos dir, dir)
    Nothing -> []

partB :: Input -> PartStatus Int
partB xs = Solved $ do
  let player = findSingle xs (Player North)
      route = Set.fromList . map fst $ path xs (player, North)
      grids = map (\p -> Map.insert p Obstacle xs) (Set.toList route)
      loops = map (\m -> isLoop (path m (player, North))) grids
  length $ filter id loops

isLoop :: (Ord a) => [a] -> Bool
isLoop a = go a a
  where
    go :: (Ord b) => [b] -> [b] -> Bool
    go (x : xs) (_ : y : ys) = x == y || go xs ys
    go _ _ = False

parser :: Parser Input
parser = gridify <$> (some . choice) [Obstacle <$ symbol "#", Open <$ symbol ".", Player North <$ symbol "^"] `sepBy` eol <* eof

pretty :: Point -> Text
pretty Obstacle = "#"
pretty Open = "."
pretty (Player North) = "^"
pretty (Player East) = "›"
pretty (Player West) = "‹"
pretty (Player South) = "∨"
pretty (Player _) = "*"

day06 :: AoC Input Int
day06 = mkAoC parser partA partB 6 2024
