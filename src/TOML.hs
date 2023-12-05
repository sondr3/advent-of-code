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
import Text.Megaparsec.Char (alphaNumChar, space1, spaceChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Universum hiding (try)

data Answer = Answer
  { p1 :: Maybe Int,
    p2 :: Maybe Int
  }
  deriving stock (Eq, Show)

data Input = Input
  { comment :: Maybe Text,
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
textBetween bt = bt (toText <$> some (alphaNumChar <|> spaceChar))

quoted :: Parser a -> Parser a
quoted = between (symbol "\"") (symbol "\"")

doubleBrackets :: Parser a -> Parser a
doubleBrackets = between (symbol "[[") (symbol "]]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parseAnswers :: Parser Answer
parseAnswers = braces $ Answer <$> ansParser "p1" <* optional (symbol ",") <*> ansParser "p2"
  where
    ansParser :: Text -> Parser (Maybe Int)
    ansParser p = optional (symbol p *> symbol "=" *> lexeme L.decimal)

parseTextBlock :: Parser Text
parseTextBlock = symbol "\"\"\"" >> toText <$> manyTill L.charLiteral (symbol "\"\"\"")

parseInput :: Parser Input
parseInput = do
  void $ doubleBrackets (symbol "input")
  comment <- optional (symbol "comment" >> symbol "=" >> lexeme (textBetween quoted))
  ans <- lexeme "answers" >> symbol "=" >> parseAnswers
  input <- lexeme "input" >> symbol "=" >> parseTextBlock
  pure $ Input comment ans input
