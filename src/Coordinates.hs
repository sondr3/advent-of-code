module Coordinates
  ( Dir (..),
    Position,
    allDirs,
    diagonals,
    cardinals,
    move,
    line,
    allPos,
  )
where

data Dir
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving stock (Show, Eq, Ord, Enum, Bounded)

type Position = (Int, Int)

allDirs :: [Dir]
allDirs = [minBound .. maxBound]

diagonals :: [Dir]
diagonals = [NorthEast, SouthEast, SouthWest, NorthWest]

cardinals :: [Dir]
cardinals = [North, East, South, West]

move :: Position -> Dir -> Position
move (x, y) North = (x, y - 1)
move (x, y) NorthEast = (x + 1, y - 1)
move (x, y) East = (x + 1, y)
move (x, y) SouthEast = (x + 1, y + 1)
move (x, y) South = (x, y + 1)
move (x, y) SouthWest = (x - 1, y + 1)
move (x, y) West = (x - 1, y)
move (x, y) NorthWest = (x - 1, y - 1)

-- get all positions that form a line in a direction of length `n`
line :: Position -> Dir -> Int -> [Position]
line pos dir n = take n $ iterate (`move` dir) pos

-- generate all (x, y) coordinates across a grid
allPos :: Position -> Position -> [Position]
allPos (minX, minY) (maxX, maxY) = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
