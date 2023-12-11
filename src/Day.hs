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

import Chronos (SubsecondPrecision (SubsecondPrecisionAuto), encodeTimespan, stopwatch)
import Parsers (Parser, parseInput, testParseInput)
import PrettyPrint (prettyPrint)
import TOML (Document, Input, answers, comment, input, inputs, p1, p2, parseDocument)
import Text.Megaparsec (errorBundlePretty, runParser)
import Universum hiding ((^.))
import Utils (padNum)

runDay :: Int -> Int -> AoC -> IO ()
runDay day year MkAoC {solve} = solve day year

testParseExample :: (Show a) => Int -> Int -> Parser a -> IO ()
testParseExample d year parser = do
  res <- parseExample d year parser

  case res of
    Left err -> error $ "Failed to parse input: " <> err
    Right ast -> prettyPrint ast

parseExample :: Int -> Int -> Parser a -> IO (Either Text a)
parseExample d year parser = do
  day <- getDayDocument d year
  pure $ testParseInput parser (Just "example") (input $ head $ inputs day)

testParseDay :: (Show a) => Int -> Int -> Parser a -> IO ()
testParseDay d year parser = do
  day <- getDayDocument d year
  forM_ (inputs day) $ \i -> do
    case testParseInput parser (comment i) (input i) of
      Left err -> error $ "Failed to parse input: " <> err
      Right ast -> prettyPrint ast

getDayDocument :: Int -> Int -> IO Document
getDayDocument day year = do
  file <- readFile filename
  case runParser parseDocument filename file of
    Left err -> error $ "Failed to parse TOML file: " <> toText (errorBundlePretty err)
    Right doc -> pure doc
  where
    filename = toString $ "inputs/" <> show year <> "/day" <> padNum day <> ".toml"

runPart :: (i -> Int) -> i -> Int -> Int -> IO ()
runPart solver i expected part = do
  (duration, res) <- stopwatch (pure $ solver i)

  when (res /= expected) $ do
    putTextLn $ "Part " <> show part <> " failed: expected " <> show expected <> " but got " <> show res

  putTextLn $ "   Part " <> show part <> ": " <> show res <> " in " <> encodeTimespan SubsecondPrecisionAuto duration <> "ms"

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
        putTextLn $ "Solution for " <> show year <> ", day " <> padNum day
        docs <- getDayDocument day year
        forM_ (inputs docs) $ \input -> do
          whenJust (comment input) $ \c -> putTextLn $ ">> " <> c
          solveInput input (mkAoC p p1 p2 d y)
    }
