{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsers where

import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M hiding (getInput)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer qualified as L
import Universum
import Utils (padNum)

type Parser = Parsec Void Text

data InputKind = Example | Input

inputExtension :: InputKind -> Text
inputExtension Example = ".example"
inputExtension Input = ".input"

inputName :: Int -> InputKind -> FilePath
inputName day kind = show $ "inputs/day" <> padNum day <> inputExtension kind

getInput :: Int -> InputKind -> Parser a -> IO a
getInput i kind p = do
  input <- readFile $ inputName i kind
  pLines p input

number :: (Integral a) => Parser a
number = L.signed pass L.decimal

pLines :: (MonadThrow m) => Parser a -> Text -> m a
pLines parser input = case M.parse (M.many eol *> parser <* M.many eol) "" input of
  Left err -> throwM err
  Right a -> pure a

getRight :: Either a b -> b
getRight (Right x) = x
getRight _ = error "getRight called with Left value"
