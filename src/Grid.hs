module Grid
  ( gridify,
    printGrid,
    printGridMap,
    createGrid,
  )
where

import Data.Map qualified as Map
import Data.Text qualified as T
import Universum

gridify :: [[a]] -> Map (Int, Int) a
gridify xs = Map.fromList [((j, i), x) | (i, row) <- zip [0 ..] xs, (j, x) <- zip [0 ..] row]

printGrid :: (MonadIO m) => [[a]] -> (a -> Text) -> m ()
printGrid xs prettify = mapM_ (putTextLn . T.concat) (createGrid (map prettify $ gridify xs))

printGridMap :: (MonadIO m) => Map (Int, Int) a -> (a -> Text) -> m ()
printGridMap grid prettify = mapM_ (putTextLn . T.concat) (createGrid (Map.map prettify grid))

createGrid :: Map (Int, Int) a -> [[a]]
createGrid grid =
  let (maxX, maxY) = fst $ Map.findMax grid
      (minX, minY) = fst $ Map.findMin grid
   in [[(Map.!) grid (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
