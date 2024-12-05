{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day04 where

import Coordinates (Dir (..), Position, allDirs, allPos, line, move)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Day (AoC, PartStatus (..), mkAoC)
import Grid (getAtPos)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char

type Row = [Char]

type Input = [Row]

gridify :: Input -> [Position]
gridify xs = allPos (0, 0) (length xs, length xs)

partA :: Input -> PartStatus
partA xs = Solved . sum $ map isXmas $ concatMap (find xs 4) (gridify xs)

isXmas :: Row -> Int
isXmas ['X', 'M', 'A', 'S'] = 1
isXmas _ = 0

find :: Input -> Int -> Position -> [Row]
find grid n pos = map go allDirs
  where
    go dir = get grid $ line pos dir n

partB :: Input -> PartStatus
partB xs = Solved $ length $ filter id $ map (\p -> cross xs p 3) (gridify xs)

get :: Input -> [Position] -> Row
get grid = mapMaybe (`getAtPos` grid)

getDir :: Input -> Position -> Dir -> Dir -> Int -> Row
getDir grid pos s e n = get grid (line (move pos s) e n)

cross :: Input -> Position -> Int -> Bool
cross grid pos n = isMas (getDir grid pos SouthWest NorthEast n) && isMas (getDir grid pos NorthWest SouthEast n)

isMas :: Row -> Bool
isMas ['M', 'A', 'S'] = True
isMas ['S', 'A', 'M'] = True
isMas _ = False

parser :: Parser Input
parser = fmap T.unpack <$> some (takeWhile1P Nothing (/= '\n') <* (eol $> () <|> eof))

day04 :: AoC Input
day04 = mkAoC parser partA partB 4 2024
