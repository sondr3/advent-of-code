{-# LANGUAGE FlexibleContexts #-}

module TestUtils (testDay, testInput) where

import AoC (AoC (..), PartStatus, getDayPuzzle)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Day (PartStatus (..))
import Parsers (parseInput)
import Puzzle.Types
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldNotBe)
import Utils (padNum)

testDay :: AoC i Int -> Spec
testDay m@MkAoC {..} = describe (T.unpack $ "day " <> padNum day) $ do
  docs <- runIO (getDayPuzzle day year)
  forM_ (inputs docs) $ \input -> do
    testInput input m

testInput :: Input a -> AoC i a -> Spec
testInput i MkAoC {..} = do
  p <- parseInput parser (comment i) (input i)
  let n = fromMaybe "input" (name i)

  it (T.unpack $ "should solve part 1 on input " <> n) $ runPart part1 p (answer1 i)
  it (T.unpack $ "should solve part 2 on input " <> n) $ runPart part2 p (answer2 i)

runPart :: (Show a, Eq a) => (i -> PartStatus a) -> i -> Answer a -> IO ()
runPart _ _ Unanswered = pure ()
runPart part i NilAnswer = part i `shouldNotBe` Unsolved
runPart part i (Answer a) = part i `shouldBe` Solved a
