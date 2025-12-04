module Grid
  ( Grid,
    padGrid,
    paddedGridPos,
    getAtPos,
    find,
    find',
    gridify,
    invertGrid,
    findOnGrid,
    findSingle,
    onGrid,
    gridSize,
    printGrid,
    printGridMap,
    createGrid,
  )
where

import Coordinates (Position)
import Data.List (transpose, (!?))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Utils (uHead)

type Grid a = Map Position a

-- Pad a grid with n items around the whole area
padGrid :: Int -> a -> [[a]] -> [[a]]
padGrid p c grid = transpose $ map pad $ transpose $ map pad grid
  where
    pad l = replicate p c ++ l ++ replicate p c

paddedGridPos :: Grid a -> Int -> (Position, Position)
paddedGridPos g p =
  let (maxX, maxY) = fst $ Map.findMax g
      (minX, minY) = fst $ Map.findMin g
   in ((minX + p, minY + p), (maxX - p, maxY - p))

getAtPos :: Position -> [[a]] -> Maybe a
getAtPos (x, y) g = do
  row <- g !? y
  row !? x

find :: Position -> Grid a -> Maybe a
find = Map.lookup

find' :: Position -> Grid a -> a
find' pos g = fromJust $ find pos g

gridify :: [[a]] -> Map (Int, Int) a
gridify xs = Map.fromList [((j, i), x) | (i, row) <- zip [0 ..] xs, (j, x) <- zip [0 ..] row]

invertGrid :: (Ord a) => Map Position a -> Map a [Position]
invertGrid = Map.foldrWithKey (\k v -> Map.insertWith (++) v [k]) Map.empty

gridSize :: Map Position a -> Position
gridSize = fst . Map.findMax

findSingle :: (Eq a) => Map k a -> a -> k
findSingle xs n = fst . uHead . Map.toList $ findOnGrid xs n

onGrid :: (Ord k) => Map k a -> k -> Bool
onGrid grid k = isJust $ Map.lookup k grid

findOnGrid :: (Eq a) => Map k a -> a -> Map k a
findOnGrid xs n = Map.filter (== n) xs

printGrid :: [[a]] -> (a -> Text) -> IO ()
printGrid xs prettify = mapM_ (TIO.putStrLn . T.concat) (createGrid (prettify <$> gridify xs))

printGridMap :: Map (Int, Int) a -> (a -> Text) -> IO ()
printGridMap grid prettify = mapM_ (TIO.putStrLn . T.concat) (createGrid (Map.map prettify grid))

createGrid :: Map (Int, Int) a -> [[a]]
createGrid grid =
  let (maxX, maxY) = fst $ Map.findMax grid
      (minX, minY) = fst $ Map.findMin grid
   in [[(Map.!) grid (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
