module Utils
  ( padNum,
    isDigit,
    pairs,
    read,
    getRight,
    compareLengths,
    readConcat,
    pick,
    listToMaybe,
  )
where

import Data.Text qualified as T
import Universum

padNum :: Int -> Text
padNum n = T.justifyRight 2 '0' $ show n

isDigit :: Char -> Bool
isDigit c = (fromIntegral (ord c - ord '0') :: Word) <= 9

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : b : xs) = (a, b) : pairs xs
pairs _ = error "uneven list"

readConcat :: (Read a, Show a) => [a] -> a
readConcat xs = read $ foldl (<>) "" $ map show xs

read :: (Read a) => Text -> a
read = getRight . readEither . toString

getRight :: Either a b -> b
getRight (Right x) = x
getRight _ = error "getRight called with Left value"

compareLengths :: (Container t1, Container t2) => t1 -> t2 -> Ordering
compareLengths a b = compare (length b) (length a)

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick k (x : xs) = map (x :) (pick (k - 1) xs) <> pick k xs

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe xs = Just xs
