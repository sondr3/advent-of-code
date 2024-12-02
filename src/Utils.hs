module Utils
  ( padNum,
    isDigit,
    pairs,
    read',
    getRight,
    compareLengths,
    readConcat,
    pick,
    listToMaybe,
    whenJust,
    uTail,
    uHead,
    pairwise
  )
where

import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Text.Read (readEither)

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (uTail xs)

padNum :: Int -> Text
padNum n = T.justifyRight 2 '0' $ display n

isDigit :: Char -> Bool
isDigit c = (fromIntegral (ord c - ord '0') :: Word) <= 9

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : b : xs) = (a, b) : pairs xs
pairs _ = error "uneven list"

readConcat :: (Read a, Display a) => [a] -> a
readConcat xs = read' $ foldl (<>) "" $ map display xs

read' :: (Read a) => Text -> a
read' = getRight . readEither . T.unpack

getRight :: Either a b -> b
getRight (Right x) = x
getRight _ = error "getRight called with Left value"

compareLengths :: (Foldable t1, Foldable t2) => t2 a1 -> t1 a2 -> Ordering
compareLengths a b = compare (length b) (length a)

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick k (x : xs) = map (x :) (pick (k - 1) xs) <> pick k xs

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe xs = Just xs

whenJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _ = pure ()

uHead :: [a] -> a
uHead (x : _) = x
uHead [] = error "unsafeHead called with empty list"

uTail :: [a] -> [a]
uTail (_ : xs) = xs
uTail [] = error "unsafeTail called with empty list"
