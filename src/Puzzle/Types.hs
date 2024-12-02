module Puzzle.Types
  ( Answer (..),
    Puzzle (..),
    Input (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

data Answer = Unanswered | NilAnswer | Answer Int
  deriving stock (Eq, Show)

newtype Puzzle = Puzzle {inputs :: NonEmpty Input}
  deriving stock (Show, Eq)

data Input = Input
  { part1 :: Answer,
    part2 :: Answer,
    comment :: Maybe Text,
    name :: Maybe Text,
    input :: Text
  }
  deriving stock (Show, Eq)
