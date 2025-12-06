module Puzzle.Parser
  ( parsePuzzle,
    parseHeader,
    parseInput,
  )
where

import Control.Applicative.Combinators.NonEmpty qualified as NE
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Puzzle.Types
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, eol, space1, spaceChar)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipLineComment "--")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

comma :: Parser Text
comma = symbol ","

equals :: Parser Text
equals = symbol "="

quoted :: Parser a -> Parser a
quoted = between (symbol "\"") (symbol "\"")

text :: Parser Text
text = T.pack <$> some (alphaNumChar <|> spaceChar)

quote :: Parser Text
quote = quoted text

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parseBody :: Parser Text
parseBody = T.dropWhileEnd (== '\n') . T.concat <$> some (notFollowedBy (chunk "#{") >> takeWhileP (Just "body") (/= '\n') <> eol <?> "File body")

parsePuzzle :: Parser Puzzle
parsePuzzle = Puzzle <$> NE.some parseInput <* eof

parseInput :: Parser Input
parseInput = do
  (p1, p2, name, cmt) <- parseHeader
  Input p1 p2 cmt name <$> parseBody

parseHeader :: Parser (Answer, Answer, Maybe Text, Maybe Text)
parseHeader = do
  void (symbol "#" <?> "hash")
  braces $ do
    name <- parseText "name" <* optional comma
    p1 <- (parseAnswer "p1" <|> pure Unanswered) <* optional comma
    p2 <- (parseAnswer "p2" <|> pure Unanswered) <* optional comma
    cmt <- parseText "comment"
    pure (p1, p2, name, cmt)

parseText :: Text -> Parser (Maybe Text)
parseText t = try . optional $ symbol t *> equals *> choice [quote, text]

parseAnswer :: Text -> Parser Answer
parseAnswer p = symbol p *> equals *> choice [IntAnswer <$> lexeme L.decimal, NilAnswer <$ symbol "nil"]
