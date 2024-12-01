module Grid
  ( gridify,
    printGrid,
    printGridMap,
    createGrid,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

gridify :: [[a]] -> Map (Int, Int) a
gridify xs = Map.fromList [((j, i), x) | (i, row) <- zip [0 ..] xs, (j, x) <- zip [0 ..] row]

printGrid :: [[a]] -> (a -> Text) -> IO ()
printGrid xs prettify = mapM_ (TIO.putStrLn . T.concat) (createGrid (prettify <$> gridify xs))

printGridMap :: Map (Int, Int) a -> (a -> Text) -> IO ()
printGridMap grid prettify = mapM_ (TIO.putStrLn . T.concat) (createGrid (Map.map prettify grid))

createGrid :: Map (Int, Int) a -> [[a]]
createGrid grid =
  let (maxX, maxY) = fst $ Map.findMax grid
      (minX, minY) = fst $ Map.findMin grid
   in [[(Map.!) grid (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
