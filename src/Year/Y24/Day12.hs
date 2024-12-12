{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day12 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import Day (Day (..))
import GHC.Generics (Generic)
import Grid (Grid, gridify)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

newtype Plot = Plot Char
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

type Input = Grid Plot

partA :: Input -> Answer
partA xs = Unanswered

partB :: Input -> Answer
partB xs = Unanswered

parser :: Parser Input
parser = gridify <$> (some (Plot <$> anySingleBut '\n')) `sepBy` eol

day12 :: AoC Input
day12 = mkAoC parser partA partB D12 Y24
