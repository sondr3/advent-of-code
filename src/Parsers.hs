{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsers where

import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M hiding (getInput)
import Text.Megaparsec.Char (eol, hspace)
import Text.Megaparsec.Char.Lexer qualified as L
import Universum

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: Text -> Parser Text
symbol = L.symbol hspace

number :: (Integral a) => Parser a
number = L.signed pass L.decimal

pLines :: (Applicative f) => Parser a -> Text -> f a
pLines parser input = case M.parse (M.many eol *> parser <* M.many eol) "" input of
  Left err -> error (toText $ M.errorBundlePretty err)
  Right a -> pure a

getRight :: Either a b -> b
getRight (Right x) = x
getRight _ = error "getRight called with Left value"
