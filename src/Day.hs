{-# LANGUAGE ScopedTypeVariables #-}

module Day
  ( AoC (..),
    mkAoC,
    runDay,
    getDayDocument,
    getAoCDocument,
    testParseDay,
  )
where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.IO qualified as TIO
import GHC.Base (when)
import Parsers (Parser, parseInput, testParseInput)
import System.CPUTime (getCPUTime)
import TOML (Document, Input, answers, comment, input, inputs, p1, p2, parseDocument)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.Printf (printf)
import Utils (padNum, whenJust)

runDay :: AoC -> IO ()
runDay MkAoC {solve, year, day} = solve day year

testParseDay :: AoC -> IO ()
testParseDay m@MkAoC {parse} = do
  day <- getAoCDocument m
  forM_ (inputs day) $ \i -> do
    case testParseInput parse (comment i) (input i) of
      Left err -> error $ "Failed to parse input: " <> err
      Right _ -> pure ()

getAoCDocument :: AoC -> IO Document
getAoCDocument MkAoC {day, year} = getDayDocument day year

getDayDocument :: Int -> Int -> IO Document
getDayDocument day year = do
  file <- TIO.readFile filename
  case runParser parseDocument filename file of
    Left err -> error $ "Failed to parse TOML file: " <> errorBundlePretty err
    Right doc -> pure doc
  where
    filename = T.unpack $ "inputs/" <> display year <> "/day" <> padNum day <> ".toml"

stopwatch :: IO a -> IO (Double, a)
stopwatch action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) * 1e-9
  return (diff, result)

runPart :: (i -> Int) -> i -> Int -> Int -> IO ()
runPart solver i expected part = do
  (duration, res) <- stopwatch (pure $ solver i)

  when (res /= expected) $ do
    TIO.putStrLn $ "Part " <> display part <> " failed: expected " <> display expected <> " but got " <> display res

  TIO.putStrLn $ "   Part " <> display part <> ": " <> display res <> " in " <> T.pack (printf "%.3fs" duration)

solveInput :: Input -> AoC -> IO ()
solveInput i MkAoC {parse, part1, part2} = do
  parsed <- parseInput parse (comment i) (input i)

  whenJust (p1 $ answers i) $ \p -> runPart part1 parsed p 1
  whenJust (p2 $ answers i) $ \p -> runPart part2 parsed p 2

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
        docs <- getDayDocument day year
        forM_ (inputs docs) $ \input -> do
          whenJust (comment input) $ \c -> TIO.putStrLn $ ">> " <> c
          solveInput input (mkAoC p p1 p2 d y)
    }
