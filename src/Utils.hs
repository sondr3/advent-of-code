module Utils (padNum) where

import Data.Text qualified as T
import Universum

padNum :: Int -> Text
padNum n = T.justifyRight 2 '0' $ show n
