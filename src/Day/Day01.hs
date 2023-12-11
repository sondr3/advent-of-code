{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day01 where

import Data.Text qualified as T
import Data.Text.Read (decimal)
import Day (AoC, mkAoC)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Universum hiding (some)
import Universum.Unsafe (fromJust)
import Utils (getRight, isDigit)

partA :: [Text] -> Int
partA xs = sum $ map fst $ rights $ map (decimal . (\t -> toText [T.head t, T.head (T.reverse t)]) . T.filter isDigit) xs

partB :: [Text] -> Int
partB xs = sum $ map ((\x -> fst . getRight $ decimal $ T.concat [head x, last x]) . extractNums) xs

wordsToNum :: [(Text, Text)]
wordsToNum = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

extractNums :: Text -> NonEmpty Text
extractNums input = fromJust $ nonEmpty (go input)
  where
    go :: Text -> [Text]
    go t
      | T.null t = []
      | T.head t `elem` ['0' .. '9'] = T.singleton (T.head t) : go (T.tail t)
      | otherwise = case matchNum wordsToNum t of
          Just (num, rest) -> num : go rest
          Nothing -> go $ T.tail t

matchNum :: [(Text, Text)] -> Text -> Maybe (Text, Text)
matchNum [] _ = Nothing
matchNum ((word, num) : rest) t =
  if word `T.isPrefixOf` t
    then Just (num, T.tail t)
    else matchNum rest t

parser :: Parser [Text]
parser = M.many $ takeWhile1P Nothing (/= '\n') <* optional eol

day01 :: AoC
day01 = mkAoC parser partA partB 1 2023
