{-# LANGUAGE FlexibleContexts #-}

module TestUtils (testDay, testInput) where

import AoC (AoC (..), getDayDocument, mkAoC)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Parsers (parseInput)
import TOML
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldNotBe)
import Utils (padNum)

testDay :: AoC -> Spec
testDay MkAoC {parse, part1, part2, day, year} = describe (T.unpack $ "day " <> padNum day) $ do
  docs <- runIO (getDayDocument day year)
  forM_ (inputs docs) $ \input -> do
    testInput input (mkAoC parse part1 part2 day year)

testInput :: Input -> AoC -> Spec
testInput i MkAoC {parse, part1, part2} = do
  parsed <- parseInput parse (comment i) (input i)

  let name = fromMaybe "input" (comment i)
  it (T.unpack $ "should parse " <> name) $ do
    runPart part1 parsed (p1 $ answers i)
    runPart part2 parsed (p2 $ answers i)

runPart :: (i -> Int) -> i -> Answer -> IO ()
runPart _ _ Unanswered = pure ()
runPart part i NilAnswer = part i `shouldNotBe` 0
runPart part i (Answer a) = part i `shouldBe` a
