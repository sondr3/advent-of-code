{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day10 where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Day (AoC, mkAoC)
import Grid (gridify)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char hiding (string)
import Universum
import Universum.Unsafe qualified as U

data Cell = Start | Ground | Pipe (Dir, Dir) deriving stock (Show, Eq, Ord)

prettyCell :: Cell -> Text
prettyCell Start = "S"
prettyCell Ground = "."
prettyCell (Pipe (North, South)) = "│"
prettyCell (Pipe (East, West)) = "─"
prettyCell (Pipe (North, East)) = "└"
prettyCell (Pipe (North, West)) = "┘"
prettyCell (Pipe (South, West)) = "┐"
prettyCell (Pipe (South, East)) = "┌"
prettyCell _ = "?"

data Dir = North | South | East | West deriving stock (Show, Eq, Ord)

move :: (Int, Int) -> Dir -> (Int, Int)
move (x, y) North = (x, y - 1)
move (x, y) South = (x, y + 1)
move (x, y) East = (x + 1, y)
move (x, y) West = (x - 1, y)

dfs :: Map (Int, Int) Cell -> [(Int, Int)]
dfs g = go (filterGround g) (findStart g) Set.empty [] ++ [findStart g]
  where
    go grid pos seen visited
      | Set.member pos seen = []
      | otherwise = pos : concat [go grid (move pos dir) (Set.insert pos seen) (pos : visited) | dir <- validDirs currentCell]
      where
        currentCell = Map.findWithDefault Ground pos grid
        validDirs Start = [East]
        validDirs (Pipe (dir1, dir2)) = [dir1, dir2]
        validDirs Ground = []

filterGround :: Map (Int, Int) Cell -> Map (Int, Int) Cell
filterGround = Map.filter (/= Ground)

findStart :: Map (Int, Int) Cell -> (Int, Int)
findStart m = U.head $ Map.keys $ Map.filter (== Start) m

partA :: [[Cell]] -> Int
partA xs = length (dfs $ gridify xs) `div` 2

shoelaceArea :: [(Int, Int)] -> Int
shoelaceArea pts = abs $ (`div` 2) $ sum [x1 * y2 - y1 * x2 | ((x1, y1), (x2, y2)) <- zip pts (U.tail pts ++ [U.head pts])]

cellsPretty :: Map (Int, Int) Cell -> Map (Int, Int) Text
cellsPretty = Map.map prettyCell

partB :: [[Cell]] -> Int
partB xs = (abs (shoelaceArea area * 2) - length area - 1 + 3) `div` 2
  where
    area = dfs (gridify xs)

parser :: Parser [[Cell]]
parser = pipeParser `sepEndBy` eol

pipeParser :: Parser [Cell]
pipeParser =
  (some . choice)
    [ Pipe (North, South) <$ char '|',
      Pipe (East, West) <$ char '-',
      Pipe (North, East) <$ char 'L',
      Pipe (North, West) <$ char 'J',
      Pipe (South, West) <$ char '7',
      Pipe (South, East) <$ char 'F',
      Start <$ char 'S',
      Ground <$ char '.'
    ]

day10 :: AoC
day10 = mkAoC parser partA partB 10 2023
