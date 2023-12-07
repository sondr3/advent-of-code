module Day
  ( AoC (..),
    mkAoC,
    runDay,
    getDayDocument,
    testParseDay,
    testParseExample,
    parseExample,
  )
where

import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Parsers (Parser, parseInput, testParseInput)
import PrettyPrint (prettyPrint)
import TOML (Document, Input, answers, comment, input, inputs, p1, p2, parseDocument)
import Text.Megaparsec (errorBundlePretty, runParser)
import Universum hiding ((^.))
import Utils (padNum)

diffTime :: UTCTime -> UTCTime -> Text
diffTime start end = show $ diffUTCTime end start

runDay :: Int -> AoC -> IO ()
runDay day MkAoC {solve} = solve day

testParseExample :: (Show a) => Int -> Parser a -> IO ()
testParseExample d parser = do
  res <- parseExample d parser

  case res of
    Left err -> error $ "Failed to parse input: " <> err
    Right ast -> prettyPrint ast

parseExample :: Int -> Parser a -> IO (Either Text a)
parseExample d parser = do
  day <- getDayDocument d
  pure $ testParseInput parser (Just "example") (input $ head $ inputs day)

testParseDay :: (Show a) => Int -> Parser a -> IO ()
testParseDay d parser = do
  day <- getDayDocument d
  forM_ (inputs day) $ \i -> do
    case testParseInput parser (comment i) (input i) of
      Left err -> error $ "Failed to parse input: " <> err
      Right ast -> prettyPrint ast

getDayDocument :: Int -> IO Document
getDayDocument day = do
  file <- readFile filename
  case runParser parseDocument filename file of
    Left err -> error $ "Failed to parse TOML file: " <> toText (errorBundlePretty err)
    Right doc -> pure doc
  where
    filename = toString $ "inputs/day" <> padNum day <> ".toml"

runPart :: (i -> Int) -> i -> Int -> Int -> IO ()
runPart solver i expected part = do
  start <- getCurrentTime
  let res = solver i
  stop <- getCurrentTime

  when (res /= expected) $ do
    error $ "Part " <> show part <> " failed: expected " <> show expected <> " but got " <> show res

  putTextLn $ "  Part " <> show part <> ": " <> show res <> " in " <> diffTime start stop

solveInput :: Input -> AoC -> IO ()
solveInput i MkAoC {parse, part1, part2} = do
  parsed <- parseInput parse (comment i) (input i)

  whenJust (p1 $ answers i) $ \p -> runPart part1 parsed p 1
  whenJust (p2 $ answers i) $ \p -> runPart part2 parsed p 2

data AoC = forall i.
  (Show i) =>
  MkAoC
  { parse :: Parser i,
    part1 :: i -> Int,
    part2 :: i -> Int,
    solve :: Int -> IO ()
  }

mkAoC :: (Show i) => Parser i -> (i -> Int) -> (i -> Int) -> AoC
mkAoC p p1 p2 =
  MkAoC
    { parse = p,
      part1 = p1,
      part2 = p2,
      solve = \day -> do
        putTextLn $ "Solution for day " <> padNum day
        docs <- getDayDocument day
        forM_ (inputs docs) $ \input -> do
          whenJust (comment input) $ \c -> putTextLn $ ">> " <> c
          solveInput input (mkAoC p p1 p2)
    }
