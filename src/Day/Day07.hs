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

data JokerCard = J' | Two' | Three' | Four' | Five' | Six' | Seven' | Eight' | Nine' | T' | Q' | K' | A'
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Hand = HighCard | Pair | TwoPair | Threes | FullHouse | FourOfAKind | FiveOfAKind
  deriving stock (Show, Eq, Ord)

cardToJokerCard :: Card -> JokerCard
cardToJokerCard Two = Two'
cardToJokerCard Three = Three'
cardToJokerCard Four = Four'
cardToJokerCard Five = Five'
cardToJokerCard Six = Six'
cardToJokerCard Seven = Seven'
cardToJokerCard Eight = Eight'
cardToJokerCard Nine = Nine'
cardToJokerCard T = T'
cardToJokerCard J = J'
cardToJokerCard Q = Q'
cardToJokerCard K = K'
cardToJokerCard A = A'

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

jokerHands :: Hand -> [JokerCard] -> Hand
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
    numJ = length $ filter (== J') cs

buildHand :: [([Card], c)] -> [(Hand, [Card], c)]
buildHand xs = zipWith (\a (b, c) -> (a, b, c)) (map (cardsToHand . fst) xs) xs

partA :: [([Card], Int)] -> Int
partA xs = sum $ zipWith (curry (\((_, _, bid), rnk) -> rnk * bid)) (sortBy compareHands $ buildHand xs) [1 ..]

partB :: [([Card], Int)] -> Int
partB xs = sum $ zipWith (curry (\((_, _, bid), rnk) -> rnk * bid)) (sortBy compareHands $ map (\(h, cs, bid) -> (jokerHands h (map cardToJokerCard cs), map cardToJokerCard cs, bid)) $ buildHand xs) [1 ..]

parser :: Parser [([Card], Int)]
parser = some $ do
  cards <- some cardParser <* hspace
  bid <- number <* optional eol
  pure (cards, bid)

cardParser :: Parser Card
cardParser = choice [A <$ char 'A', K <$ char 'K', Q <$ char 'Q', J <$ char 'J', T <$ char 'T', Nine <$ char '9', Eight <$ char '8', Seven <$ char '7', Six <$ char '6', Five <$ char '5', Four <$ char '4', Three <$ char '3', Two <$ char '2']

day07 :: AoC
day07 = mkAoC parser partA partB
