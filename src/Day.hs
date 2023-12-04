module Day (AoC, mkAoC, runDay) where

import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Parsers (Parser, pLines)
import Universum
import Utils (padNum)

diffTime :: UTCTime -> UTCTime -> Text
diffTime start end = show $ diffUTCTime end start

runDay :: Int -> Text -> AoC -> IO ()
runDay day input MkAoC {solve} = solve day input

data AoC = forall i o.
  (Eq o, Show o) =>
  MkAoC
  { parse :: Parser i,
    part1 :: i -> o,
    part2 :: i -> o,
    answers :: NonEmpty [o],
    solve :: Int -> Text -> IO ()
  }

mkAoC :: (Eq o, Show o) => Parser i -> (i -> o) -> (i -> o) -> NonEmpty [o] -> AoC
mkAoC p p1 p2 ans =
  MkAoC
    { parse = p,
      part1 = p1,
      part2 = p2,
      answers = ans,
      solve = \day input -> do
        parsed <- pLines p input

        p1Time <- getCurrentTime
        let p1Res = p1 parsed
        p1Elapsed <- getCurrentTime

        p2Time <- getCurrentTime
        let p2Res = p2 parsed
        p2Elapsed <- getCurrentTime

        putTextLn $ "Solution for day " <> padNum day
        putTextLn $ "  Part 1: " <> show p1Res <> " in " <> diffTime p1Elapsed p1Time
        putTextLn $ "  Part 2: " <> show p2Res <> " in " <> diffTime p2Elapsed p2Time
    }