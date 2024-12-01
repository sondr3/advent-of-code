module TOML
  ( Answers (..),
    Answer (..),
    Input (..),
    Document (..),
    getInputs,
    getTextInputAt,
    parseDocument,
    parseTitle,
    parseAnswers,
    parseDocumentInput,
    whenAnswer,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char (alphaNumChar, space1, spaceChar, string)
import Text.Megaparsec.Char.Lexer qualified as L

data Answer = Unanswered | NilAnswer | Answer Int
  deriving stock (Eq, Show)

data Answers = Answers
  { p1 :: Answer,
    p2 :: Answer
  }
  deriving stock (Eq, Show)

whenAnswer :: (Applicative f) => Answer -> (Answer -> f ()) -> f ()
whenAnswer a@(Answer _) f = f a
whenAnswer a@NilAnswer f = f a
whenAnswer Unanswered _ = pure ()

data Input = Input
  { comment :: Maybe Text,
    answers :: Answers,
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
parseDocument = Document <$> parseTitle <*> NE.some parseDocumentInput

parseTitle :: Parser Text
parseTitle = symbol "title" *> symbol "=" *> lexeme (textBetween quoted)

textBetween :: (Parser Text -> Parser Text) -> Parser Text
textBetween bt = bt (T.pack <$> some (alphaNumChar <|> spaceChar))

quoted :: Parser a -> Parser a
quoted = between (symbol "\"") (symbol "\"")

doubleBrackets :: Parser a -> Parser a
doubleBrackets = between (symbol "[[") (symbol "]]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parseAnswers :: Parser Answers
parseAnswers = braces $ Answers <$> ansParser "p1" <* optional (symbol ",") <*> ansParser "p2"
  where
    ansParser :: Text -> Parser Answer
    ansParser p = (symbol p *> symbol "=" *> choice [Answer <$> lexeme L.decimal, NilAnswer <$ symbol "nil"]) <|> pure Unanswered

parseTextBlock :: Parser Text
parseTextBlock = string "\"\"\"" >> T.strip . T.pack <$> manyTill L.charLiteral (symbol "\"\"\"")

parseDocumentInput :: Parser Input
parseDocumentInput = do
  void $ doubleBrackets (symbol "input")
  comment <- optional (symbol "comment" >> symbol "=" >> lexeme (textBetween quoted))
  ans <- lexeme "answers" >> symbol "=" >> parseAnswers
  input <- lexeme "input" >> symbol "=" >> parseTextBlock
  pure $ Input comment ans input

getInputs :: Document -> NonEmpty Input
getInputs Document {inputs} = inputs

getTextInputAt :: Int -> Document -> Text
getTextInputAt i Document {inputs} = T.strip $ input $ toList inputs !! i
