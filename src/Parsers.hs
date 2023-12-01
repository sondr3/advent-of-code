{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsers where

import Data.Text (isSuffixOf)
import System.Directory (getCurrentDirectory, listDirectory)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M hiding (getInput)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer qualified as L
import Universum
import Utils (padNum)

type Parser = Parsec Void Text

data Part
  = PartA
  | PartB
  deriving stock (Eq, Show, Ord, Enum, Bounded)

data InputKind
  = Example
  | Input
  deriving stock (Eq, Show, Ord, Enum, Bounded)

inputExtension :: InputKind -> Text
inputExtension Example = ".example"
inputExtension Input = ".input"

filepathToKind :: FilePath -> Bool
filepathToKind fp = any (\e -> inputExtension e `isSuffixOf` toText fp) $ enumFrom Example

allDays :: [(InputKind, Part, Int)]
allDays = sort' $ [(kind, part, day) | kind <- enumFrom Example, part <- enumFrom PartA, day <- [1 .. 25]]
  where
    sort' :: [(InputKind, Part, Int)] -> [(InputKind, Part, Int)]
    sort' = sortBy (\(_, p1, a) (_, p2, b) -> compare (a, p1) (b, p2))

getInputs :: IO (NonEmpty FilePath)
getInputs = do
  dir <- getCurrentDirectory
  fromList . filter filepathToKind <$> listDirectory (dir <> "/inputs")

numDays :: IO Int
numDays = fmap (flip div 2 . length) getInputs

numInputs :: IO (NonEmpty (NonEmpty (InputKind, Part, Int)))
numInputs = fromList . groupBy ((==) `on` (\(_, _, d) -> d)) <$> inputs
  where
    inputs :: IO [(InputKind, Part, Int)]
    inputs = take <$> fmap (* 4) numDays <*> pure allDays

inputName :: Int -> InputKind -> FilePath
inputName day kind = toString $ "inputs/day" <> padNum day <> inputExtension kind

parseInput :: Int -> InputKind -> Parser a -> IO a
parseInput i kind p = do
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
