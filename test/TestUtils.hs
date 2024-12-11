{-# LANGUAGE FlexibleContexts #-}

module TestUtils (testDay, testInput) where

import AoC (AoC (..), getDayPuzzle)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Parsers (parseInput)
import Puzzle.Types (Answer (..), Input (..), Puzzle (..))
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldNotBe)
import Utils (padNum)

testDay :: AoC i -> Spec
testDay m@MkAoC {..} = describe (T.unpack $ "day " <> padNum day) $ do
  docs <- runIO (getDayPuzzle day year)
  forM_ (inputs docs) $ \input -> do
    testInput input m

testInput :: Input -> AoC i -> Spec
testInput i MkAoC {..} = do
  p <- parseInput parser (comment i) (input i)
  let n = fromMaybe "input" (name i)

  it (T.unpack $ "should solve part 1 on input " <> n) $ runPart part1 p (answer1 i)
  it (T.unpack $ "should solve part 2 on input " <> n) $ runPart part2 p (answer2 i)

runPart :: (i -> Answer) -> i -> Answer -> IO ()
runPart _ _ Unanswered = pure ()
runPart part i NilAnswer = part i `shouldNotBe` Unanswered
runPart part i (IntAnswer a) = part i `shouldBe` IntAnswer a
