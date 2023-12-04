module TOML
  ( Answer (..),
    Input (..),
    Document (..),
    parseDocument,
    parseTitle,
    parseAnswers,
    parseInput,
  )
where

import Control.Monad.Combinators.NonEmpty qualified as NE
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char (alphaNumChar, char, space1, spaceChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Universum

data Answer = Answer
  { p1 :: Int,
    p2 :: Int
  }
  deriving stock (Eq, Show)

data Input = Input
  { name :: Text,
    answers :: Answer,
    input :: Text
  }
  deriving stock (Eq, Show)

data Document = Document
  { title :: Text,
    inputs :: NonEmpty Input
  }
  deriving stock (Eq, Show)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipLineComment "#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parseDocument :: Parser Document
parseDocument = Document <$> parseTitle <*> NE.some parseInput

parseTitle :: Parser Text
parseTitle = symbol "title" *> symbol "=" *> lexeme (textBetween quoted)

textBetween :: (Parser Text -> Parser Text) -> Parser Text
textBetween bt = bt (toText <$> some (alphaNumChar <|> spaceChar <|> char '_'))

quoted :: Parser a -> Parser a
quoted = between (symbol "\"") (symbol "\"")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parseAnswers :: Parser Answer
parseAnswers = braces $ Answer <$> ansParser "p1" <* symbol "," <*> ansParser "p2"
  where
    ansParser :: Text -> Parser Int
    ansParser p = symbol p *> symbol "=" *> lexeme L.decimal

parseTextBlock :: Parser Text
parseTextBlock = symbol "\"\"\"" >> toText <$> manyTill L.charLiteral (symbol "\"\"\"")

parseInput :: Parser Input
parseInput = do
  name <- lexeme (textBetween brackets)
  ans <- lexeme "answers" >> symbol "=" >> parseAnswers
  input <- lexeme "input" >> symbol "=" >> parseTextBlock
  pure $ Input name ans input
