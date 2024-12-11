{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day08 where

import Coordinates (Position, distPos, unitVector)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Day (AoC, mkAoC)
import Grid (gridSize, gridify, invertGrid)
import Parsers (Parser, symbol)
import Puzzle.Types (Answer (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils (combinations, uHead)

data Point
  = Open
  | Antenna Char
  | Antinode
  deriving stock (Show, Eq, Ord)

isAntenna :: Point -> Bool
isAntenna (Antenna _) = True
isAntenna _ = False

type Input = Map Position Point

partA :: Input -> Answer
partA = IntAnswer . countAntinodes . run (\(a, b) -> [uHead a, uHead b])

partB :: Input -> Answer
partB xs =
  IntAnswer $
    (\m -> countAntennas m + countAntinodes m) $
      run (\(a, b) -> takeWhile (isOnGrid xs) a ++ takeWhile (isOnGrid xs) b) xs

run :: (([Position], [Position]) -> [Position]) -> Input -> Input
run f xs = placeAntinodes xs $ findAntinodes f $ antennaPos xs

countAntinodes :: Input -> Int
countAntinodes = Map.size . Map.filter (== Antinode)

countAntennas :: Input -> Int
countAntennas = Map.size . Map.filter isAntenna

antennaPos :: Input -> Map Point [Position]
antennaPos xs = Map.filterWithKey (\k _ -> k /= Open) (invertGrid xs)

findAntinodes :: (([Position], [Position]) -> [Position]) -> Map Point [Position] -> [Position]
findAntinodes f xs = concatMap (f . (\[a, b] -> antinodes a b)) (concatMap (combinations 2) $ Map.elems xs)

placeAntinodes :: Input -> [Position] -> Input
placeAntinodes = foldl' (\grid p -> putOnGrid grid (p, Antinode))

putOnGrid :: Input -> (Position, Point) -> Input
putOnGrid g (pos, p) = if isJust $ Map.lookup pos g then Map.insert pos p g else g

isOnGrid :: Input -> Position -> Bool
isOnGrid g (x, y) = (x <= maxX && x >= 0) && (y <= maxY && y >= 0)
  where
    (maxX, maxY) = gridSize g

antinodes :: Position -> Position -> ([Position], [Position])
antinodes p1@(x1, y1) p2@(x2, y2) = unzip $ map makePos ([1 ..] :: [Int])
  where
    (ux, uy) = unitVector p1 p2
    d = distPos p1 p2
    makePos n = ((x1 - dx, y1 - dy), (x2 + dx, y2 + dy))
      where
        dx = round (d * ux * fromIntegral n)
        dy = round (d * uy * fromIntegral n)

parser :: Parser Input
parser = gridify <$> (some . choice) [Open <$ symbol "." <|> Antenna <$> anySingleBut '\n'] `sepBy` eol <* eof

pretty :: Point -> Text
pretty Open = "."
pretty (Antenna c) = T.singleton c
pretty Antinode = "#"

day08 :: AoC Input
day08 = mkAoC parser partA partB 8 2024
