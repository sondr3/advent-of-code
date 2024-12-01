{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsers where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec qualified as M hiding (getInput)
import Text.Megaparsec.Char (alphaNumChar, hspace)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: Text -> Parser Text
symbol = L.symbol hspace

number :: (Integral a) => Parser a
number = L.signed (pure ()) L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

string :: Parser Text
string = T.pack <$> lexeme (some alphaNumChar)

testParseInput :: Parser i -> Maybe Text -> Text -> Either String i
testParseInput parser name input = case M.parse parser (T.unpack $ fromMaybe "input" name) (T.strip input) of
  Left err -> Left $ M.errorBundlePretty err
  Right a -> Right a

parseInput :: (Applicative f) => Parser a -> Maybe Text -> Text -> f a
parseInput parser name input = either error pure $ testParseInput parser name input
