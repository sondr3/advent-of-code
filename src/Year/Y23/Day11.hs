{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y23.Day11 where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Day (Answer (..), AoC, Year (..), mkAoC)
import GHC.Generics (Generic)
import Grid (gridify)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char hiding (string)
import Utils (pick)

data Space
  = Empty
  | Galaxy
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

prettySpace :: Space -> Text
prettySpace Empty = "."
prettySpace Galaxy = "#"

partA :: Map (Int, Int) Space -> Answer
partA xs = IntAnswer . sum $ map manhattan' $ pick 2 $ calculate (findGalaxies xs) 2

partB :: Map (Int, Int) Space -> Answer
partB xs = IntAnswer . sum $ map manhattan' $ pick 2 $ calculate (findGalaxies xs) 1000000

manhattan' :: [(Int, Int)] -> Int
manhattan' [a, b] = manhattan a b
manhattan' _ = error "manhattan' called with list of length /= 2"

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

findGalaxies :: Map (Int, Int) Space -> [(Int, Int)]
findGalaxies m = Map.keys $ Map.filter (== Galaxy) m

empty' :: [(Int, Int)] -> ((Int, Int) -> Int) -> Set Int
empty' gals f = Set.difference (Set.fromList [0 .. maximum xs]) (Set.fromList xs)
  where
    xs = map f gals

calculate :: [(Int, Int)] -> Int -> [(Int, Int)]
calculate gals expand = map go gals
  where
    emptyCols = empty' gals fst
    emptyRows = empty' gals snd
    go (x, y) = (x', y')
      where
        x' = x + (expand - 1) * Set.size (Set.filter (< x) emptyCols)
        y' = y + (expand - 1) * Set.size (Set.filter (< y) emptyRows)

parser :: Parser (Map (Int, Int) Space)
parser = gridify <$> some (choice [Empty <$ char '.', Galaxy <$ char '#']) `sepEndBy` eol

day11 :: AoC (Map (Int, Int) Space)
day11 = mkAoC parser partA partB 11 Y23
