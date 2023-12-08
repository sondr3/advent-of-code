{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsers where

import Data.Text qualified as T
import Text.Megaparsec (Parsec, between)
import Text.Megaparsec qualified as M hiding (getInput)
import Text.Megaparsec.Char (alphaNumChar, hspace)
import Text.Megaparsec.Char.Lexer qualified as L
import Universum

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: Text -> Parser Text
symbol = L.symbol hspace

number :: (Integral a) => Parser a
number = L.signed pass L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

string :: Parser Text
string = toText <$> lexeme (some alphaNumChar)

testParseInput :: Parser a -> Maybe Text -> Text -> Either Text a
testParseInput parser name input = case M.parse parser (toString $ fromMaybe "input" name) (T.strip input) of
  Left err -> Left $ toText (M.errorBundlePretty err)
  Right a -> Right a

parseInput :: (Applicative f) => Parser a -> Maybe Text -> Text -> f a
parseInput parser name input = either error pure $ testParseInput parser name input
