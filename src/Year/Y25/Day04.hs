{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y25.Day04 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Control.DeepSeq (NFData)
import Coordinates (Position, allDirs, neighbours)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Day (Day (..))
import GHC.Generics (Generic)
import Grid (Grid, find, gridify)
import Parsers (Parser, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char

data Cell = Paper | Empty
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

type Input = Grid Cell

pretty :: Cell -> Text
pretty Paper = "@"
pretty Empty = "."

partA :: Input -> Answer
partA xs = IntAnswer . length . fst $ clear xs

partB :: Input -> Answer
partB xs = IntAnswer . length $ concatMap fst $ takeWhile (\(c, _) -> not . null $ c) $ iterate (\(_, acc) -> clear acc) (clear xs)

clear :: Input -> ([Cell], Input)
clear xs = Map.mapAccumWithKey go [] xs
  where
    go acc pos Paper = if numPapers pos xs < 4 then (Paper : acc, Empty) else (acc, Paper)
    go acc _ Empty = (acc, Empty)

numPapers :: Position -> Input -> Int
numPapers pos g = length $ filter (== Just Paper) $ filter isJust $ papers g pos

papers :: Input -> Position -> [Maybe Cell]
papers g pos = map (`find` g) (neighbours pos allDirs)

parser :: Parser Input
parser = gridify <$> (some (Paper <$ symbol "@" <|> Empty <$ symbol ".") `sepBy` eol) <* eof

day04 :: AoC Input
day04 = mkAoC parser partA partB D4 Y25
