{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y24.Day09 where

import Control.DeepSeq (NFData)
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Display (display)
import Day (Answer (..), AoC, Year (..), mkAoC)
import GHC.Generics (Generic)
import Parsers (Parser)
import Text.Megaparsec

data FileSystem
  = Free
  | File Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

value :: FileSystem -> Int
value Free = 0
value (File n) = n

isFile :: FileSystem -> Bool
isFile (File _) = True
isFile _ = False

type Input = Seq FileSystem

partA :: Input -> Answer
partA xs = IntAnswer . sum $ zipWith (\f i -> i * value f) (toList $ sortFiles xs) [0 ..]

partB :: Input -> Answer
partB _ = Unanswered

sortFiles :: Input -> Input
sortFiles xs = do
  let (ends, files) = Seq.breakr isFile xs
      fstFree = fromJust $ Seq.findIndexL (not . isFile) xs
      lstFile = fromJust $ Seq.findIndexR isFile xs
   in if all isFile files && (not . any isFile) ends
        then xs
        else
          sortFiles (swap lstFile fstFree xs)

swap :: Int -> Int -> Seq a -> Seq a
swap i1 i2 xs =
  let v1 = Seq.index xs i1
      v2 = Seq.index xs i2
   in Seq.update i2 v1 $ Seq.update i1 v2 xs

parser :: Parser Input
parser = Seq.fromList . file [0 ..] <$> some (digitToInt <$> anySingle) <* eof
  where
    file _ [] = []
    file [] _ = []
    file (y : ys) (x : xs) = replicate x (File y) ++ free ys xs
    free _ [] = []
    free ys (x : xs) = replicate x Free ++ file ys xs

pretty :: FileSystem -> Text
pretty Free = "."
pretty (File i) = display i

day09 :: AoC Input
day09 = mkAoC parser partA partB 9 Y24
