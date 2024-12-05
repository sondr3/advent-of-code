{-# LANGUAGE FlexibleContexts #-}

module TestUtils (testDay, testInput) where

import AoC (AoC (..), PartStatus, getDayPuzzle, mkAoC)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Day (PartStatus (..))
import Parsers (parseInput)
import Puzzle.Types
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldNotBe)
import Utils (padNum)

testDay :: AoC i -> Spec
testDay MkAoC {parser, part1, part2, day, year} = describe (T.unpack $ "day " <> padNum day) $ do
  docs <- runIO (getDayPuzzle day year)
  forM_ (inputs docs) $ \input -> do
    testInput input (mkAoC parser part1 part2 day year)

testInput :: Input -> AoC i -> Spec
testInput i MkAoC {parser, part1, part2} = do
  parsed <- parseInput parser (comment i) (input i)

  let name = fromMaybe "input" (comment i)
  it (T.unpack $ "should parse " <> name) $ do
    runPart part1 parsed (answer1 i)
    runPart part2 parsed (answer2 i)

runPart :: (i -> PartStatus) -> i -> Answer -> IO ()
runPart _ _ Unanswered = pure ()
runPart part i NilAnswer = part i `shouldNotBe` Unsolved
runPart part i (Answer a) = part i `shouldBe` Solved a
