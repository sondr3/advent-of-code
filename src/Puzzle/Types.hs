module Puzzle.Types
  ( Answer (..),
    Puzzle (..),
    Input (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.Display (Display)

data Answer a where
  Unanswered :: (Eq a, Display a) => Answer a
  NilAnswer :: (Eq a, Display a) => Answer a
  Answer :: (Eq a, Display a) => a -> Answer a

deriving stock instance (Eq a) => Eq (Answer a)

deriving stock instance (Show a) => Show (Answer a)

type role Answer nominal

newtype Puzzle a = Puzzle {inputs :: NonEmpty (Input a)}
  deriving stock (Show, Eq)

type role Puzzle nominal

data Input a = Input
  { answer1 :: Answer a,
    answer2 :: Answer a,
    comment :: Maybe Text,
    name :: Maybe Text,
    input :: Text
  }
  deriving stock (Show, Eq)

type role Input nominal
