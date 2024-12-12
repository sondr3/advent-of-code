{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Year.Y23.Day08 where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Day (Answer (..), AoC, Year (..), mkAoC)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char hiding (string)
import Utils (uHead)

data Dir = R | L
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

step :: [Dir] -> Map Text (Text, Text) -> Text -> [Text]
step [] _ _ = error "impossible"
step (L : ds) m xs = xs : step ds m (fst $ m M.! xs)
step (R : ds) m xs = xs : step ds m (snd $ m M.! xs)

partA :: ([Dir], Map Text (Text, Text)) -> Answer
partA (dirs, nodes) = IntAnswer . length $ takeWhile (/= "ZZZ") $ step (cycle dirs) nodes "AAA"

partB :: ([Dir], Map Text (Text, Text)) -> Answer
partB (dirs, nodes) = IntAnswer $ foldr (lcm . (length . takeWhile (not . isEndNode) . step (cycle dirs) nodes)) 1 startNodes
  where
    startNodes = filter isStartNode $ M.keys nodes
    isStartNode n = "A" `T.isSuffixOf` n
    isEndNode n = "Z" `T.isSuffixOf` n

parser :: Parser ([Dir], Map Text (Text, Text))
parser = do
  d <- dirParser <* some eol
  nodes <- M.fromList . sortWith fst <$> (nodeParser `sepBy` eol)
  pure (d, nodes)

nodeParser :: Parser (Text, (Text, Text))
nodeParser = do
  root <- string <* symbol "="
  edges <- parens (string `sepBy` symbol ",")
  pure (root, (uHead edges, last edges))

dirParser :: Parser [Dir]
dirParser = some $ choice [R <$ char 'R', L <$ char 'L']

day08 :: AoC ([Dir], Map Text (Text, Text))
day08 = mkAoC parser partA partB 8 Y23
