module Utils
  ( allEq,
    combinations,
    compareLengths,
    dropped,
    getRight,
    isDigit,
    listToMaybe,
    nDigits,
    padNum,
    pairs,
    pairwise,
    chunksOf,
    pick,
    read',
    intToText,
    textToInt,
    readConcat,
    splitNum,
    tupToList,
    uHead,
    uTail,
    whenJust,
  )
where

import Data.Char (ord)
import Data.List (subsequences, unfoldr)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.Builder.Int qualified as BI
import Data.Text.Read (decimal)
import Text.Read (readEither)

-- create a sliding window over a list
pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (uTail xs)

tupToList :: (a, a) -> [a]
tupToList (x, y) = [x, y]

nDigits :: (Integral a) => a -> Int
nDigits n = floor (logBase (10 :: Double) (fromIntegral n)) + 1

splitNum :: (Integral a) => a -> (a, a)
splitNum n = divMod n (10 ^ (nDigits n `div` 2))

dropped :: [a] -> [[a]]
dropped xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

padNum :: Int -> Text
padNum n = T.justifyRight 2 '0' $ display n

isDigit :: Char -> Bool
isDigit c = (fromIntegral (ord c - ord '0') :: Word) <= 9

-- are all elements equal in the list
allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x : xs) = all (== x) xs

-- take two and two items from a list
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : b : xs) = (a, b) : pairs xs
pairs _ = error "uneven list"

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k ==) . length) $ subsequences ns

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = takeWhile (not . null) $ unfoldr (Just . splitAt n) xs

readConcat :: (Read a, Display a) => [a] -> a
readConcat xs = read' $ foldl (<>) "" $ map display xs

read' :: (Read a) => Text -> a
read' = getRight . readEither . T.unpack

intToText :: (Integral a) => a -> T.Text
intToText = TL.toStrict . B.toLazyText . BI.decimal

textToInt :: T.Text -> Int
textToInt = fst . getRight . decimal

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
