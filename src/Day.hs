{-# LANGUAGE ScopedTypeVariables #-}

module Day
  ( AoC (..),
    mkAoC,
    runDay,
    getDayPuzzle,
    getAoCPuzzle,
    getAoCDocument,
    getDayDocument,
    testParseDay,
  )
where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.IO qualified as TIO
import Parsers (Parser, parseInput, testParseInput)
import PrettyPrint (prettyPrint)
import Puzzle.Parser (parsePuzzle)
import Puzzle.Types (Answer (..), Input (..), Puzzle (..))
import System.CPUTime (getCPUTime)
import System.OsPath (decodeUtf, unsafeEncodeUtf)
import TOML (Document, parseDocument)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.Printf (printf)
import Utils (padNum, whenJust)

runDay :: AoC -> IO ()
runDay MkAoC {solve, year, day} = solve day year

testParseDay :: AoC -> IO ()
testParseDay m@MkAoC {parse} = do
  day <- getAoCPuzzle m
  forM_ (inputs day) $ \i -> do
    case testParseInput parse (comment i) (input i) of
      Left err -> error $ "Failed to parse input: " <> err
      Right v -> prettyPrint v

getAoCPuzzle :: AoC -> IO Puzzle
getAoCPuzzle MkAoC {day, year} = getDayPuzzle day year

getAoCDocument :: AoC -> IO Document
getAoCDocument MkAoC {day, year} = getDayDocument day year

getDayDocument :: Int -> Int -> IO Document
getDayDocument day year = do
  fname <- decodeUtf filename
  file <- TIO.readFile fname
  case runParser parseDocument fname file of
    Left err -> error $ "Failed to parse TOML file: " <> errorBundlePretty err
    Right doc -> pure doc
  where
    filename = unsafeEncodeUtf $ "inputs/" <> show year <> "/day" <> T.unpack (padNum day) <> ".toml"

getDayPuzzle :: Int -> Int -> IO Puzzle
getDayPuzzle day year = do
  fname <- decodeUtf filename
  file <- TIO.readFile fname
  case runParser parsePuzzle fname file of
    Left err -> error $ "Failed to parse puzzle file: " <> errorBundlePretty err
    Right doc -> pure doc
  where
    filename = unsafeEncodeUtf $ "inputs/" <> show year <> "/day" <> T.unpack (padNum day) <> ".aoc"

stopwatch :: IO a -> IO (Double, a)
stopwatch action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) * 1e-9
  return (diff, result)

runPart :: (i -> Int) -> i -> Answer -> Int -> IO ()
runPart solver i answer part = do
  (duration, res) <- stopwatch (pure $ solver i)

  TIO.putStr $ "> part " <> display part
  case answer of
    Unanswered -> TIO.putStr " is as of yet unanswered"
    NilAnswer -> TIO.putStr $ " had `nil` as answer, but got " <> display res
    Answer e ->
      if res /= e
        then TIO.putStr $ " failed: expected " <> display e <> " but got " <> display res
        else
          TIO.putStr $ " correct: " <> display res <> " in " <> T.pack (printf "%.3fs" duration)

  TIO.putStrLn ""

solveInput :: Input -> AoC -> IO ()
solveInput i MkAoC {parse, part1, part2} = do
  parsed <- parseInput parse (name i) (input i)

  runPart part1 parsed (answer1 i) 1
  runPart part2 parsed (answer2 i) 2

data AoC
  = forall i.
  (Show i) =>
  MkAoC
  { parse :: Parser i,
    part1 :: i -> Int,
    part2 :: i -> Int,
    day :: Int,
    year :: Int,
    solve :: Int -> Int -> IO ()
  }

mkAoC ::
  (Show i) =>
  -- | Parser
  Parser i ->
  -- | Part 1
  (i -> Int) ->
  -- | Part 2
  (i -> Int) ->
  -- | Day
  Int ->
  -- | Year
  Int ->
  AoC
mkAoC p p1 p2 d y =
  MkAoC
    { parse = p,
      part1 = p1,
      part2 = p2,
      day = d,
      year = y,
      solve = \day year -> do
        TIO.putStrLn $ "Solution for " <> display year <> ", day " <> padNum day
        docs <- getDayPuzzle day year
        forM_ (inputs docs) $ \input -> do
          whenJust (comment input) $ \c -> TIO.putStrLn $ "$ " <> c
          solveInput input (mkAoC p p1 p2 d y)
    }
