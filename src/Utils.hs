module Utils (padNum, isDigit, pairs) where

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
