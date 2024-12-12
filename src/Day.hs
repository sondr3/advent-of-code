{-# LANGUAGE DerivingVia #-}

module Day
  ( Day (..),
    padDay,
  )
where

import Data.Text (Text)
import Data.Text.Display (Display, ShowInstance (..))
import Utils (padNum, uTail)

data Day
  = D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  | D10
  | D11
  | D12
  | D13
  | D14
  | D15
  | D16
  | D17
  | D18
  | D19
  | D20
  | D21
  | D22
  | D23
  | D24
  | D25
  deriving stock (Show, Eq, Ord, Enum, Bounded, Read)
  deriving (Display) via (ShowInstance Day)

padDay :: Day -> Text
padDay d = padNum (read $ uTail $ show d)
