{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day09 where

import AoC (uHead, uTail)
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as Set
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..), (><), (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Day (AoC, PartStatus (..), mkAoC)
import Debug.Trace (trace, traceId, traceShow, traceShowId, traceWith)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data FileSystem
  = Free
  | File Int
  deriving stock (Show, Eq)

value :: FileSystem -> Int
value (Free) = 0
value (File n) = n

isFile :: FileSystem -> Bool
isFile (File _) = True
isFile _ = False

type Input = Seq FileSystem

partA :: Input -> PartStatus Int
partA xs = Solved . sum $ zipWith (\f i -> i * value f) (toList $ sortFiles xs) [0 ..]

partB :: Input -> PartStatus Int
partB xs = Unsolved

sortFilesFit :: IntSet -> Input -> Input
sortFilesFit seen xs = do
  let (ends, files) = traceShowId $ Seq.breakr isFile xs
   in if all isFile files && all (not . isFile) ends
        then xs
        else do
          let frs = traceShowId $ Seq.takeWhileL (not . isFile) xs
              fs@(f :<| _) = traceShowId $ undefined -- Seq.takeWhileR (\f -> isFile f && not . Set.member (value f) seen) xs
           in if Seq.length fs > length frs
                then sortFilesFit (Set.insert (value f) seen) xs
                else sortFilesFit seen xs

takeFile :: Seq FileSystem -> Seq FileSystem
takeFile Seq.Empty = Seq.empty
takeFile ((Free :<| xs)) = Seq.empty
takeFile (f@(File _) :<| Seq.Empty) = Seq.singleton f
takeFile (f1@(File _) :<| Free :<| xs) = Seq.singleton f1 >< takeFile xs
takeFile (f1@(File n1) :<| f2@(File n2) :<| xs) = if n1 /= n2 then Seq.empty else Seq.fromList [f2, f1] >< takeFile xs

sortFiles :: Input -> Input
sortFiles xs = do
  let (ends, files) = Seq.breakr isFile xs
      fstFree = fromJust $ Seq.findIndexL (not . isFile) xs
      lstFile = fromJust $ Seq.findIndexR isFile xs
   in if all isFile files && all (not . isFile) ends
        then xs
        else
          sortFiles (swap lstFile fstFree xs)

swap :: Int -> Int -> Seq a -> Seq a
swap i1 i2 xs =
  let v1 = Seq.index xs i1
      v2 = Seq.index xs i2
   in Seq.update i2 v1 $ Seq.update i1 v2 xs

parser :: Parser Input
parser = Seq.fromList <$> file [0 ..] <$> some (digitToInt <$> anySingle) <* eof
  where
    file _ [] = []
    file [] _ = []
    file (y : ys) (x : xs) = replicate x (File y) ++ free ys xs
    free _ [] = []
    free ys (x : xs) = replicate x Free ++ file ys xs

pretty :: FileSystem -> Text
pretty (Free) = "."
pretty (File i) = display i

day09 :: AoC Input Int
day09 = mkAoC parser partA partB 9 2024
