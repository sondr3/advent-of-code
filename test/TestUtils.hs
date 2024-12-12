{-# LANGUAGE FlexibleContexts #-}

module TestUtils (testDay, testInput) where

import AoC (AoC (..), getDayPuzzle)
import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Parsers (parseInput)
import Puzzle.Types (Answer (..), Input (..), Puzzle (..))
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldNotBe)
import Utils (padNum)

testDay :: (Show i, NFData i) => AoC i -> Spec
testDay m@AoC {..} = describe (T.unpack $ "day " <> padNum day) $ do
  ps <- runIO (getDayPuzzle day year)
  forM_ (puzzles ps) $ \input -> do
    testInput input m

testInput :: (Show i, NFData i) => Input -> AoC i -> Spec
testInput i AoC {..} = do
  p <- parseInput parser (comment i) (input i)
  let n = fromMaybe "input" (name i)

  it (T.unpack $ "should solve part 1 on input " <> n) $ runPart part1 p (answer1 i)
  it (T.unpack $ "should solve part 2 on input " <> n) $ runPart part2 p (answer2 i)

runPart :: (i -> Answer) -> i -> Answer -> IO ()
runPart _ _ Unanswered = pure ()
runPart part i NilAnswer = part i `shouldNotBe` Unanswered
runPart part i (IntAnswer a) = part i `shouldBe` IntAnswer a
