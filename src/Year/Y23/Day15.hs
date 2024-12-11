{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y23.Day15 where

import AoC (read')
import Control.Applicative (Alternative (..))
import Data.Char (ord)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Day (AoC, mkAoC)
import Parsers
import Puzzle.Types (Answer (..))
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char hiding (string)

partA :: [Text] -> Answer
partA xs = IntAnswer . sum $ map hash xs

partB :: [Text] -> Answer
partB xs = IntAnswer . sum $ map focusPower $ Map.toList (foldl' box buildBoxes xs)

hash :: Text -> Int
hash = T.foldl' (\acc a -> ((ord a + acc) * 17) `mod` 256) 0

focusPower :: (Int, [(Text, Int)]) -> Int
focusPower (i, xs) = sum [(i + 1) * j * snd x | (j, x) <- zip [1 ..] xs]

buildBoxes :: Map Int [(Text, Int)]
buildBoxes = Map.fromList [(i, []) | i <- [0 .. 255]]

box :: Map Int [(Text, Int)] -> Text -> Map Int [(Text, Int)]
box boxes xs = go $ T.span (`notElem` ['=', '-']) xs
  where
    go :: (Text, Text) -> Map Int [(Text, Int)]
    go (lbl, rs) = case T.uncons rs of
      Just ('-', _) -> Map.adjust (filter ((/= lbl) . fst)) (hash lbl) boxes
      Just ('=', n)
        | lbl `elem` labels -> Map.adjust (const (before ++ [(lbl, read' n)] ++ after)) (hash lbl) boxes
        | otherwise -> Map.adjust (++ [(lbl, read' n)]) (hash lbl) boxes
      _ -> error "Invalid input"
      where
        contents = (Map.!) boxes (hash lbl)
        labels = map fst contents
        (before, _ : after) = break ((== lbl) . fst) contents

parser :: Parser [Text]
parser = (T.pack <$> some (alphaNumChar <|> char '=' <|> char '-')) `sepEndBy` char ','

day15 :: AoC [Text]
day15 = mkAoC parser partA partB 15 2023
