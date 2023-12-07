module Utils (padNum, isDigit, pairs, intListToText, read, getRight) where

import Data.Text qualified as T
import Data.Text.Read (decimal)
import Universum

padNum :: Int -> Text
padNum n = T.justifyRight 2 '0' $ show n

isDigit :: Char -> Bool
isDigit c = (fromIntegral (ord c - ord '0') :: Word) <= 9

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : b : xs) = (a, b) : pairs xs
pairs _ = error "uneven list"

intListToText :: (Show (Element c), Container c, Integral a) => c -> a
intListToText i = fst $ getRight $ decimal $ toText $ concatMap show i

read :: (Read a) => Text -> a
read = getRight . readEither . toString

getRight :: Either a b -> b
getRight (Right x) = x
getRight _ = error "getRight called with Left value"
