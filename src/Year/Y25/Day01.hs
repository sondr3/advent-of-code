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
partA xs = IntAnswer . length . filter (== 0) $ build xs

partB :: Input -> Answer
partB xs = IntAnswer $ sum . map (abs . fst) $ scanl (\(_, curr) (op, n) -> go op curr n `divMod` 100) (0, 50) xs
  where

parser :: Parser Input
parser = liftA2 (,) (L <$ symbol "L" <|> R <$ symbol "R") (lexeme L.decimal) `sepBy` eol

day01 :: AoC Input
day01 = mkAoC parser partA partB D1 Y25
