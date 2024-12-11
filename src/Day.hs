{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

module Day
  ( AoC (..),
    mkAoC,
    allParsedInput,
    parsedExample,
    parsedInput,
    parsedInputN,
    runDay,
    benchmark,
    getDayPuzzle,
    getAoCPuzzle,
    testParseDay,
  )
where

import Control.Exception (handle)
import Control.Monad (forM_)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.IO qualified as TIO
import GHC.IO.Exception (ExitCode)
import Parsers (Parser, parseInput, testParseInput)
import PrettyPrint (prettyPrint)
import Puzzle.Parser (parsePuzzle)
import Puzzle.Types (Answer (..), Input (..), Puzzle (..))
import System.OsPath (decodeUtf, unsafeEncodeUtf)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)
import Text.Megaparsec (errorBundlePretty, runParser)
import Utils (padNum, uHead, whenJust)

runDay :: AoC i -> IO ()
runDay MkAoC {solve, year, day} = solve day year

testParseDay :: (Show i) => AoC i -> IO ()
testParseDay m@MkAoC {parser} = do
  day <- getAoCPuzzle m
  forM_ (inputs day) $ \i -> do
    case testParseInput parser (comment i) (input i) of
      Left err -> error $ "Failed to parse input: " <> err
      Right v -> prettyPrint (show v)

getAoCPuzzle :: AoC i -> IO Puzzle
getAoCPuzzle MkAoC {day, year} = getDayPuzzle day year

allParsedInput :: AoC i -> IO [i]
allParsedInput MkAoC {parsed} = parsed

parsedExample :: AoC i -> IO i
parsedExample MkAoC {parsed} = uHead <$> parsed

parsedInputN :: AoC i -> Int -> IO i
parsedInputN MkAoC {parsed} i = do
  inputs <- parsed
  if i >= length inputs
    then error "invalid input"
    else pure $ inputs !! i

parsedInput :: AoC i -> IO i
parsedInput MkAoC {parsed} = last <$> parsed

getDayPuzzle :: Int -> Int -> IO Puzzle
getDayPuzzle day year = do
  fname <- decodeUtf filename
  file <- TIO.readFile fname
  case runParser parsePuzzle fname file of
    Left err -> error $ "Failed to parse puzzle file: " <> errorBundlePretty err
    Right doc -> pure doc
  where
    filename = unsafeEncodeUtf $ "inputs/" <> show year <> "/day" <> T.unpack (padNum day) <> ".aoc"

runPart :: (i -> Answer) -> i -> Answer -> Int -> IO ()
runPart solver i answer part = do
  let res = solver i
  if res == Unanswered
    then
      pure ()
    else do
      TIO.putStr $ "> part " <> display part
      case answer of
        Unanswered -> TIO.putStr " is as of yet unanswered"
        NilAnswer -> TIO.putStr $ " had `nil` as answer, but got " <> display res
        IntAnswer e ->
          if res /= undefined
            then TIO.putStr $ " failed: expected " <> display e <> " but got " <> display res
            else
              TIO.putStr $ " correct: " <> display res
      TIO.putStrLn ""

benchmark :: AoC i -> IO ()
benchmark a@MkAoC {..} = do
  input <- parsedInput a
  puzzle <- getAoCPuzzle a

  handle
    ((\_ -> pure ()) :: ExitCode -> IO ())
    ( defaultMain
        [ bgroup
            ("AoC " <> show year <> " - " <> T.unpack (padNum day))
            [ bench "part 1" $ nf (`p1` puzzle) input,
              bench "part 2" $ nf (`p2` puzzle) input
            ]
        ]
    )
  where
    p1 i p = part1 i == answer1 (NE.last $ inputs p)
    p2 i p = part2 i == answer1 (NE.last $ inputs p)

solveInput :: Input -> Parser i -> (i -> Answer) -> (i -> Answer) -> IO ()
solveInput i parser part1 part2 = do
  parsed <- parseInput parser (name i) (input i)

  runPart part1 parsed (answer1 i) 1
  runPart part2 parsed (answer2 i) 2

data AoC i where
  MkAoC ::
    (Show i) =>
    { parser :: Parser i,
      parsed :: IO [i],
      part1 :: i -> Answer,
      part2 :: i -> Answer,
      day :: Int,
      year :: Int,
      solve :: Int -> Int -> IO ()
    } ->
    AoC i

type role AoC nominal

parseDay :: (Applicative f) => Parser b -> Input -> f b
parseDay p (Input {input, name}) = either error pure $ testParseInput p name input

mkAoC ::
  forall i.
  (Show i) =>
  -- | Parser
  Parser i ->
  -- | Part 1
  (i -> Answer) ->
  -- | Part 2
  (i -> Answer) ->
  -- | Day
  Int ->
  -- | Year
  Int ->
  AoC i
mkAoC p p1 p2 d y =
  MkAoC
    { parser = p,
      parsed = do
        docs <- getDayPuzzle d y
        mapM (parseDay p) (NE.toList $ inputs docs),
      part1 = p1,
      part2 = p2,
      day = d,
      year = y,
      solve = \day year -> do
        TIO.putStrLn $ "Answer for " <> display year <> ", day " <> padNum day
        docs <- getDayPuzzle day year
        forM_ (inputs docs) $ \input -> do
          whenJust (comment input) $ \c -> TIO.putStrLn $ "$ " <> c
          solveInput input p p1 p2
    }
