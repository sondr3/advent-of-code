{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y25.Day02 where

import AoC (Answer (..), AoC, Year (..), mkAoC)
import Data.Text (Text)
import Data.Text qualified as T
import Day (Day (..))
import Parsers (Parser, lexeme, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L
import Utils (allEq, intToText, textToInt)

type Input = [(Int, Int)]

partA :: Input -> Answer
partA xs = IntAnswer . sum $ map (textToInt . uncurry T.append) (filter (uncurry (==)) . map (\t -> T.splitAt (T.length t `div` 2) t) $ conv xs)

partB :: Input -> Answer
partB xs = IntAnswer $ sum $ map (textToInt . fst) $ filter snd $ map ((\(s, ys) -> (s, any (allEq . (`T.chunksOf` s)) ys)) . (\t -> (t, [1 .. T.length t `div` 2]))) (conv xs)

parser :: Parser Input
parser = liftA2 (,) (lexeme L.decimal <* symbol "-") (lexeme L.decimal) `sepBy` symbol ","

conv :: Input -> [Text]
conv = concatMap (map intToText . uncurry enumFromTo)

day02 :: AoC Input
day02 = mkAoC parser partA partB D2 Y25
