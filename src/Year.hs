{-# LANGUAGE DerivingVia #-}

module Year
  ( Year (..),
    longYear,
    parseYear,
    yearFromInt,
  )
where

import Data.Text (Text)
import Data.Text.Display (Display, ShowInstance (..))

data Year
  = Y23
  | Y24
  | Y25
  deriving stock (Read)
  deriving (Display) via (ShowInstance Year)

instance Show Year where
  show Y23 = "Y23"
  show Y24 = "Y24"
  show Y25 = "Y25"

longYear :: Year -> Text
longYear Y23 = "2023"
longYear Y24 = "2024"
longYear Y25 = "2025"

parseYear :: Text -> Maybe Year
parseYear y
  | y == "23" || y == "2023" = Just Y23
  | y == "24" || y == "2024" = Just Y24
  | y == "25" || y == "2025" = Just Y25
  | otherwise = Nothing

yearFromInt :: Int -> Maybe Year
yearFromInt y
  | y == 23 || y == 2023 = Just Y23
  | y == 24 || y == 2024 = Just Y24
  | y == 25 || y == 2025 = Just Y25
  | otherwise = Nothing
