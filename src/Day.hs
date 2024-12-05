{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

module Day
  ( AoC (..),
    PartStatus (..),
    mkAoC,
    allParsedInput,
    parsedExample,
    parsedInput,
    parsedInputN,
    runDay,
    getDayPuzzle,
    getAoCPuzzle,
    testParseDay,
  )
where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Criterion.Measurement (getCPUTime, getTime, initializeTime, secs)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display, ShowInstance (..), display)
import Data.Text.IO qualified as TIO
import Parsers (Parser, parseInput, testParseInput)
import PrettyPrint (prettyPrint)
import Puzzle.Parser (parsePuzzle)
import Puzzle.Types (Answer (..), Input (..), Puzzle (..))
import System.OsPath (decodeUtf, unsafeEncodeUtf)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.Printf (printf)
import Utils (padNum, uHead, whenJust)

runDay :: AoC i -> IO ()
runDay MkAoC {solve, year, day} = solve day year

testParseDay :: AoC i -> IO ()
testParseDay m@MkAoC {parser} = do
  day <- getAoCPuzzle m
  forM_ (inputs day) $ \i -> do
    case testParseInput parser (comment i) (input i) of
      Left err -> error $ "Failed to parse input: " <> err
      Right v -> prettyPrint v

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

runPart :: (i -> PartStatus) -> i -> Answer -> Int -> IO ()
runPart solver i answer part = do
  let res = solver i
  if res == Unsolved
    then
      pure ()
    else do
      TIO.putStr $ "> part " <> display part
      case answer of
        Unanswered -> TIO.putStr " is as of yet unanswered"
        NilAnswer -> TIO.putStr $ " had `nil` as answer, but got " <> display res
        Answer e ->
          if res /= Solved e
            then TIO.putStr $ " failed: expected " <> display e <> " but got " <> display res
            else
              TIO.putStr $ " correct: " <> display res
      TIO.putStrLn ""

solveInput :: Input -> AoC i -> IO ()
solveInput i MkAoC {parser, part1, part2} = do
  parsed <- parseInput parser (name i) (input i)

  runPart part1 parsed (answer1 i) 1
  runPart part2 parsed (answer2 i) 2

data PartStatus = Solved Int | Unsolved
  deriving stock (Eq)
  deriving (Display) via (ShowInstance PartStatus)

instance Show PartStatus where
  show Unsolved = "not solved"
  show (Solved a) = show a

data AoC i where
  MkAoC ::
    (Show i) =>
    { parser :: Parser i,
      parsed :: IO [i],
      part1 :: i -> PartStatus,
      part2 :: i -> PartStatus,
      day :: Int,
      year :: Int,
      solve :: Int -> Int -> IO ()
    } ->
    AoC i

type role AoC nominal

parseDay :: (Applicative f) => Parser b -> Input -> f b
parseDay p (Input {input, name}) = either error pure $ testParseInput p name input

mkAoC ::
  (Show i) =>
  -- | Parser
  Parser i ->
  -- | Part 1
  (i -> PartStatus) ->
  -- | Part 2
  (i -> PartStatus) ->
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
        TIO.putStrLn $ "Solution for " <> display year <> ", day " <> padNum day
        docs <- getDayPuzzle day year
        forM_ (inputs docs) $ \input -> do
          whenJust (comment input) $ \c -> TIO.putStrLn $ "$ " <> c
          solveInput input (mkAoC p p1 p2 d y)
    }
