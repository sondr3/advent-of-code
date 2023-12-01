module Utils (padNum, isDigit) where

import Data.Text qualified as T
import Universum

padNum :: Int -> Text
padNum n = T.justifyRight 2 '0' $ show n

isDigit :: Char -> Bool
isDigit c = (fromIntegral (ord c - ord '0') :: Word) <= 9
