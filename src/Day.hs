{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}

module Day
  ( AoC (..),
    PartStatus (..),
    Solvable (..),
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
import Data.Text.Display (Display, ShowInstance (..), display)
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

runDay :: AoC i a -> IO ()
runDay MkAoC {solve, year, day} = solve day year

testParseDay :: (Show i) => AoC i Int -> IO ()
testParseDay m@MkAoC {parser} = do
  day <- getAoCPuzzle m
  forM_ (inputs day) $ \i -> do
    case testParseInput parser (comment i) (input i) of
      Left err -> error $ "Failed to parse input: " <> err
      Right v -> prettyPrint (show v)

getAoCPuzzle :: AoC i Int -> IO (Puzzle Int)
getAoCPuzzle MkAoC {day, year} = getDayPuzzle day year

allParsedInput :: AoC i a -> IO [i]
allParsedInput MkAoC {parsed} = parsed

parsedExample :: AoC i a -> IO i
parsedExample MkAoC {parsed} = uHead <$> parsed

parsedInputN :: AoC i a -> Int -> IO i
parsedInputN MkAoC {parsed} i = do
  inputs <- parsed
  if i >= length inputs
    then error "invalid input"
    else pure $ inputs !! i

parsedInput :: AoC i a -> IO i
parsedInput MkAoC {parsed} = last <$> parsed

getDayPuzzle :: Int -> Int -> IO (Puzzle Int)
getDayPuzzle day year = do
  fname <- decodeUtf filename
  file <- TIO.readFile fname
  case runParser parsePuzzle fname file of
    Left err -> error $ "Failed to parse puzzle file: " <> errorBundlePretty err
    Right doc -> pure doc
  where
    filename = unsafeEncodeUtf $ "inputs/" <> show year <> "/day" <> T.unpack (padNum day) <> ".aoc"

runPart :: (Display a, Eq a, Show a) => (i -> PartStatus a) -> i -> Answer a -> Int -> IO ()
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

checkAnswer :: PartStatus a -> Answer a -> Bool
checkAnswer (Solved a) (Answer b) = a == b
checkAnswer _ _ = error "invalid answer"

benchmark :: AoC i Int -> IO ()
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
    p1 i p = checkAnswer (part1 i) (answer1 $ NE.last $ inputs p)
    p2 i p = checkAnswer (part2 i) (answer1 $ NE.last $ inputs p)

solveInput :: Input a -> AoC i a -> IO ()
solveInput i MkAoC {parser, part1, part2} = do
  parsed <- parseInput parser (name i) (input i)

  runPart part1 parsed (answer1 i) 1
  runPart part2 parsed (answer2 i) 2

class (Eq a, Show a) => Solvable a where
  unsolved :: PartStatus a
  solved :: a -> PartStatus a

data PartStatus a where
  Solved :: (Eq a, Show a) => a -> PartStatus a
  Unsolved :: (Eq a, Show a) => PartStatus a
  deriving (Display) via (ShowInstance (PartStatus a))

type role PartStatus nominal

deriving stock instance (Eq a) => Eq (PartStatus a)

instance Show (PartStatus a) where
  show Unsolved = "not solved"
  show (Solved a) = show a

instance Solvable Int where
  unsolved = Unsolved
  solved = Solved

data AoC i a where
  MkAoC ::
    (Show i, Solvable a, Display a) =>
    { parser :: Parser i,
      parsed :: IO [i],
      part1 :: i -> PartStatus a,
      part2 :: i -> PartStatus a,
      day :: Int,
      year :: Int,
      solve :: Int -> Int -> IO ()
    } ->
    AoC i a

type role AoC nominal nominal

parseDay :: (Applicative f) => Parser b -> Input i -> f b
parseDay p (Input {input, name}) = either error pure $ testParseInput p name input

mkAoC ::
  (Show i) =>
  -- | Parser
  Parser i ->
  -- | Part 1
  (i -> PartStatus Int) ->
  -- | Part 2
  (i -> PartStatus Int) ->
  -- | Day
  Int ->
  -- | Year
  Int ->
  AoC i Int
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
