module Coordinates
  ( Dir (..),
    Position,
    Turn (..),
    turnOffset,
    turnOffsetCardinal,
    turn,
    turnCardinal,
    allDirs,
    diagonals,
    cardinals,
    distManhattan,
    distPos,
    distOrigin,
    gradient,
    furthestPos,
    unitVector,
    move,
    neighbours,
    line,
    allPos,
  )
where

data Turn
  = TLeft
  | TRight
  deriving stock (Show, Eq, Ord, Enum, Bounded)

turnOffset :: Turn -> Int
turnOffset TLeft = -1
turnOffset TRight = 1

turnOffsetCardinal :: Turn -> Int
turnOffsetCardinal TLeft = -2
turnOffsetCardinal TRight = 2

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

turn :: Dir -> Turn -> Dir
turn dir t = toEnum $ (fromEnum dir + turnOffset t) `mod` length allDirs

turnCardinal :: Dir -> Turn -> Dir
turnCardinal dir t = toEnum $ (fromEnum dir + turnOffsetCardinal t) `mod` length allDirs

allDirs :: [Dir]
allDirs = [minBound .. maxBound]

diagonals :: [Dir]
diagonals = [NorthEast, SouthEast, SouthWest, NorthWest]

cardinals :: [Dir]
cardinals = [North, East, South, West]

type Position = (Int, Int)

pow :: Int -> Int -> Int
pow x y = x ^ y

-- manhattan distance between two points
distManhattan :: Position -> Position -> Int
distManhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- distance between two points
distPos :: Position -> Position -> Double
distPos (x1, y1) (x2, y2) = sqrt (fromIntegral (pow (x2 - x1) 2 + pow (y1 - y2) 2))

-- distance from origo (center)
distOrigin :: Position -> Double
distOrigin (x, y) = sqrt (fromIntegral (pow x 2 + pow y 2))

-- gradient between two points
gradient :: (Fractional a) => (a, a) -> (a, a) -> a
gradient (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

-- find out which points is furthest away from origin
furthestPos :: Position -> Position -> Position
furthestPos p1 p2
  | distOrigin p1 >= distOrigin p2 = p1
  | otherwise = p2

-- calculate the unit vector between two points
unitVector :: Position -> Position -> (Double, Double)
unitVector (x1, y1) (x2, y2) =
  let dx = fromIntegral (x2 - x1)
      dy = fromIntegral (y2 - y1)
      mag = sqrt (dx ** 2 + dy ** 2)
   in (dx / mag, dy / mag)

move :: Position -> Dir -> Position
move (x, y) North = (x, y - 1)
move (x, y) NorthEast = (x + 1, y - 1)
move (x, y) East = (x + 1, y)
move (x, y) SouthEast = (x + 1, y + 1)
move (x, y) South = (x, y + 1)
move (x, y) SouthWest = (x - 1, y + 1)
move (x, y) West = (x - 1, y)
move (x, y) NorthWest = (x - 1, y - 1)

neighbours :: Position -> [Dir] -> [Position]
neighbours pos = map (move pos)

-- get all positions that form a line in a direction of length `n`
line :: Position -> Dir -> Int -> [Position]
line pos dir n = take n $ iterate (`move` dir) pos

-- generate all (x, y) coordinates across a grid
allPos :: Position -> Position -> [Position]
allPos (minX, minY) (maxX, maxY) = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
