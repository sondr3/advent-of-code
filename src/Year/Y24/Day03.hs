{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day03 where

import Control.DeepSeq (NFData)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Day (Answer (..), AoC, Year (..), mkAoC)
import GHC.Generics (Generic)
import Parsers (Parser, lexeme, parens, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Input = [Instruction]

data Instruction
  = Mul Int Int
  | Enable
  | Disable
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

insValue :: Instruction -> Int
insValue (Mul x y) = x * y
insValue _ = 0

partA :: Input -> Answer
partA = IntAnswer . foldl' (\acc i -> acc + insValue i) 0

partB :: Input -> Answer
partB xs = IntAnswer $ foldl' (\acc i -> acc + insValue i) 0 (go xs [] True)
  where
    go (m@(Mul _ _) : xss) acc s = if s then go xss (m : acc) s else go xss acc s
    go (Enable : xss) acc _ = go xss acc True
    go (Disable : xss) acc _ = go xss acc False
    go [] acc _ = acc

parser :: Parser Input
parser = catMaybes <$> some (go <* optional eol) <* eof
  where
    go =
      choice
        [ Just <$> try parseMul,
          Just Enable <$ try (symbol "do()"),
          Just Disable <$ try (symbol "don't()"),
          anySingle $> Nothing
        ]

parseMul :: Parser Instruction
parseMul = symbol "mul" *> parens (Mul <$> (lexeme L.decimal <* symbol ",") <*> lexeme L.decimal)

day03 :: AoC Input
day03 = mkAoC parser partA partB 3 Y24
