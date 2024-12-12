{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

module Day
  ( AoC (..),
    Answer (..),
    mkAoC,
    solve,
    benchmark,
    getDayPuzzle,
    inputs,
    parseAoC,
    puzzles,
    puzzleExample,
    puzzleInput,
    puzzleInputN,
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

parseAoC :: (Show i) => AoC i -> IO ()
parseAoC m@AoC {parser} = do
  day <- puzzle m
  forM_ (puzzles day) $ \i -> do
    case testParseInput parser (comment i) (input i) of
      Left err -> error $ "Failed to parse input: " <> err
      Right v -> prettyPrint (show v)

puzzle :: AoC i -> IO Puzzle
puzzle AoC {day, year} = getDayPuzzle day year

inputs :: (Show i) => AoC i -> IO [i]
inputs AoC {..} = do
  ps <- getDayPuzzle day year
  mapM (parseDay parser) (NE.toList $ puzzles ps)

puzzleExample :: (Show i) => AoC i -> IO i
puzzleExample aoc = uHead <$> inputs aoc

puzzleInput :: (Show i) => AoC i -> IO i
puzzleInput aoc = last <$> inputs aoc

puzzleInputN :: (Show i) => AoC i -> Int -> IO i
puzzleInputN aoc i = do
  is <- inputs aoc
  if i >= length is
    then error "invalid input"
    else pure $ is !! i

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

benchmark :: (Show i) => AoC i -> IO ()
benchmark a@AoC {..} = do
  input <- puzzleInput a
  ps <- puzzle a

  handle
    ((\_ -> pure ()) :: ExitCode -> IO ())
    ( defaultMain
        [ bgroup
            ("AoC " <> show year <> " - " <> T.unpack (padNum day))
            [ bench "part 1" $ nf (`p1` ps) input,
              bench "part 2" $ nf (`p2` ps) input
            ]
        ]
    )
  where
    p1 i p = part1 i == answer1 (NE.last $ puzzles p)
    p2 i p = part2 i == answer2 (NE.last $ puzzles p)

solveInput :: Input -> Parser i -> (i -> Answer) -> (i -> Answer) -> IO ()
solveInput i parser part1 part2 = do
  parsed <- parseInput parser (name i) (input i)

  runPart part1 parsed (answer1 i) 1
  runPart part2 parsed (answer2 i) 2

data AoC i = AoC
  { parser :: (Show i) => Parser i,
    part1 :: (Show i) => i -> Answer,
    part2 :: (Show i) => i -> Answer,
    day :: Int,
    year :: Int
  }

type role AoC nominal

instance (Show i) => Show (AoC i) where
  show AoC {..} = "AoC { year = " <> show year <> ", day = " <> show day <> " }"

parseDay :: (Applicative f) => Parser b -> Input -> f b
parseDay p (Input {input, name}) = either error pure $ testParseInput p name input

solve :: (Show i) => AoC i -> IO ()
solve AoC {..} = do
  TIO.putStrLn $ "Answer for " <> display year <> ", day " <> padNum day
  docs <- getDayPuzzle day year
  forM_ (puzzles docs) $ \input -> do
    whenJust (comment input) $ \c -> TIO.putStrLn $ "$ " <> c
    solveInput input parser part1 part2

mkAoC ::
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
  AoC
    { parser = p,
      part1 = p1,
      part2 = p2,
      day = d,
      year = y
    }
