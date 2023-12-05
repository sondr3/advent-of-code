module Day (AoC, mkAoC, runDay) where

import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Parsers (Parser, pLines)
import TOML (Document, Input, answers, comment, input, inputs, p1, p2, parseDocument)
import Text.Megaparsec (runParser)
import Universum hiding ((^.))
import Utils (padNum)

diffTime :: UTCTime -> UTCTime -> Text
diffTime start end = show $ diffUTCTime end start

runDay :: Int -> AoC -> IO ()
runDay day MkAoC {solve} = solve day

getDayDocument :: Int -> IO Document
getDayDocument day = do
  file <- readFile filename
  case runParser parseDocument filename file of
    Left err -> error $ "Failed to parse TOML file: " <> show err
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
  parsed <- pLines parse (input i)

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
