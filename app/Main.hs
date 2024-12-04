module Main (main) where

import Options.Applicative

data Cmd = Cmd
  { year :: Int,
    day :: Int
  }

main :: IO ()
main = putStrLn "Hello, Haskell!"
