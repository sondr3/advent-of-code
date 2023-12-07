{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day07 where

import Day (AoC, mkAoC)
import Parsers
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Universum
import Utils (compareLengths)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
  deriving stock (Show, Eq, Ord, Enum, Bounded)

newtype JCard = JCard Card deriving stock (Show, Eq)

instance Ord JCard where
  compare (JCard J) (JCard J) = EQ
  compare (JCard J) _ = LT
  compare _ (JCard J) = GT
  compare (JCard x) (JCard y) = compare x y

data Hand = HighCard | Pair | TwoPair | Threes | FullHouse | FourOfAKind | FiveOfAKind
  deriving stock (Show, Eq, Ord)

cardToJokerCard :: Card -> JCard
cardToJokerCard = JCard

cardsToHand :: [Card] -> Hand
cardsToHand cs = case map length cards of
  [5] -> FiveOfAKind
  [4, 1] -> FourOfAKind
  [3, 2] -> FullHouse
  [3, 1, 1] -> Threes
  [2, 2, 1] -> TwoPair
  [2, 1, 1, 1] -> Pair
  _ -> HighCard
  where
    cards :: [[Card]]
    cards = map toList $ sortBy compareLengths (group $ sort cs)

compareHands :: (Ord a, Ord b) => (a, b, c1) -> (a, b, c2) -> Ordering
compareHands (h1, cs1, _) (h2, cs2, _) = case compare h1 h2 of
  EQ -> compare cs1 cs2
  x -> x

jokerHands :: Hand -> [JCard] -> Hand
jokerHands hand cs = case (hand, numJ) of
  (FiveOfAKind, _) -> FiveOfAKind
  (FourOfAKind, n) -> if n `elem` [1, 4] then FiveOfAKind else FourOfAKind
  (FullHouse, n) -> if n `elem` [2, 3] then FiveOfAKind else FullHouse
  (Threes, n) -> if n `elem` [1, 3] then FourOfAKind else Threes
  (TwoPair, 1) -> FullHouse
  (TwoPair, 2) -> FourOfAKind
  (TwoPair, _) -> TwoPair
  (Pair, n) -> if n `elem` [1, 2] then Threes else Pair
  (HighCard, n) -> if n == 1 then Pair else HighCard
  where
    numJ = length $ filter (== JCard J) cs

buildHand :: (Card -> b) -> [([Card], c)] -> [(Hand, [b], c)]
buildHand f xs = zipWith (\a (b, c) -> (a, map f b, c)) (map (cardsToHand . fst) xs) xs

partA :: [([Card], Int)] -> Int
partA xs = sum $ zipWith (curry (\((_, _, bid), rnk) -> rnk * bid)) (sortBy compareHands $ buildHand id xs) [1 ..]

partB :: [([Card], Int)] -> Int
partB xs = sum $ zipWith (curry (\((_, _, bid), rnk) -> rnk * bid)) (sortBy compareHands $ map (\(h, cs, bid) -> (jokerHands h cs, cs, bid)) $ buildHand cardToJokerCard xs) [1 ..]

parser :: Parser [([Card], Int)]
parser = some $ (,) <$> some cardParser <* hspace <*> number <* optional eol

cardParser :: Parser Card
cardParser = choice [A <$ char 'A', K <$ char 'K', Q <$ char 'Q', J <$ char 'J', T <$ char 'T', Nine <$ char '9', Eight <$ char '8', Seven <$ char '7', Six <$ char '6', Five <$ char '5', Four <$ char '4', Three <$ char '3', Two <$ char '2']

day07 :: AoC
day07 = mkAoC parser partA partB
