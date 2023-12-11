{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day03 where

import Data.Set qualified as S
import Day (AoC, mkAoC)
import Parsers (Parser)
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Universum
import Universum.Unsafe (fromJust, (!!))
import Utils (isDigit)

data Part
  = Symbol Char
  | Period
  | Part Int (Int, Int)
  deriving stock (Show, Eq, Ord)

isSymbol :: Part -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isGear :: Part -> Bool
isGear (Symbol '*') = True
isGear _ = False

isPart :: Part -> Bool
isPart (Part _ _) = True
isPart _ = False

partValue :: Part -> Int
partValue (Part x _) = x
partValue _ = 0

partA :: [[Part]] -> Int
partA xs =
  S.foldr (\x y -> partValue x + y) 0
    $ S.fromList
    $ mapMaybe (\(cs, p) -> if any (isSymbol . (\(x, y) -> xs !! x !! y)) cs && isPart p then pure p else Nothing) (matrix xs)

partB :: [[Part]] -> Int
partB xs =
  sum
    $ map
      (product . map partValue)
      ( filter (\x -> length x == 2)
          $ map (filter isPart . ordNub) (mapMaybe (\(cs, p) -> if isGear p then pure $ map (\(x, y) -> xs !! x !! y) cs else Nothing) (matrix xs))
      )

matrix :: [[Part]] -> [([(Int, Int)], Part)]
matrix xs = zip [adjacent (length xs) dx dy | dx <- [0 .. length xs - 1], dy <- [0 .. length xs - 1]] (concat xs)

adjacent :: Int -> Int -> Int -> [(Int, Int)]
adjacent n x y = filter withinBounds [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]
  where
    withinBounds (a, b) = a >= 0 && a < n && b >= 0 && b < n

parser :: Parser [[Part]]
parser = (concat <$> some (choice [parseNum, some parseChar, some parsePeriod])) `sepEndBy1` eol

parseNum :: Parser [Part]
parseNum = do
  num <- some digitChar
  pos <- getSourcePos
  let x = unPos $ sourceLine pos
      y = unPos $ sourceColumn pos
  pure $ [Part (fromJust $ readMaybe num) (x, y) | _ <- [1 .. length num]]

parseChar :: Parser Part
parseChar = Symbol <$> satisfy (\x -> not (isDigit x) && x /= '.' && x /= '\n')

parsePeriod :: Parser Part
parsePeriod = Period <$ char '.'

day03 :: AoC
day03 = mkAoC parser partA partB 3 2023
