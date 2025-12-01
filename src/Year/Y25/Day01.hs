{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y25.Day01 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Control.DeepSeq (NFData)
import Day (Day (..))
import GHC.Generics (Generic)
import Parsers (Parser, lexeme, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Dir = L | R
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

type Input = [(Dir, Int)]

go :: Dir -> (Int -> Int -> Int)
go L = (-)
go R = (+)

build :: [(Dir, Int)] -> [Int]
build = scanl (\curr (op, n) -> go op curr n `mod` 100) 50

partA :: Input -> Answer
partA xs = IntAnswer . length . filter (== 0) $ scanl (\curr (op, n) -> go op curr n `mod` 100) 50 xs

partB :: Input -> Answer
partB xs = IntAnswer $ snd $ foldl' step (50, 0) xs
  where
    step (curr, total) (op, n) = (go op curr n `mod` 100, total + cnt op n curr)
    cnt L n curr = (curr - 1) `div` 100 - (curr - n - 1) `div` 100
    cnt R n curr = (curr + n) `div` 100 - curr `div` 100

parser :: Parser Input
parser = liftA2 (,) (L <$ symbol "L" <|> R <$ symbol "R") (lexeme L.decimal) `sepBy` eol

day01 :: AoC Input
day01 = mkAoC parser partA partB D1 Y25
