module Map
  ( fromTuples,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

fromTuples :: (Ord a) => [(a, a)] -> Map a [a]
fromTuples = foldr (\(k, v) -> Map.insertWith (++) k [v]) Map.empty
