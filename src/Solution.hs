module Solution
  ( Solution (..),
    solveSolution,
    benchmarkSolution,
  )
where

import AoC (AoC, benchmark, solve)
import Control.DeepSeq (NFData)
import Data.Text.IO.Utf8 qualified as TIO

-- wrapper type for AoC
data Solution = forall i. (Show i, NFData i) => Solution (AoC i)

instance Show Solution where
  show (Solution a) = show a

solveSolution :: Maybe Solution -> IO ()
solveSolution (Just (Solution aoc)) = solve aoc
solveSolution Nothing = TIO.putStrLn "No solution found to run"

benchmarkSolution :: Maybe Solution -> IO ()
benchmarkSolution (Just (Solution aoc)) = benchmark aoc
benchmarkSolution Nothing = TIO.putStrLn "No solution found to benchmark"
