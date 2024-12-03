module Puzzle.Convert
  ( convertTOML,
    convertFolder,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Day (getDayDocument)
import Puzzle.Types qualified as P
import Puzzle.Writer (writePuzzle)
import System.OsPath (OsPath, unsafeEncodeUtf)
import TOML (Answer (..), Answers (..), Document (..), Input (..))
import Utils (padNum)

convertFolder :: Int -> [Int] -> IO ()
convertFolder year days = do
  docs <- map convertTOML <$> mapM (`getDayDocument` year) days
  mapM_ (\(d, puz) -> writePuzzle puz (out d)) (zip days docs)
  where
    out :: Int -> OsPath
    out day = unsafeEncodeUtf $ "inputs/" <> show year <> "/day" <> T.unpack (padNum day) <> ".aoc"

convertTOML :: Document -> P.Puzzle
convertTOML (Document {inputs}) = P.Puzzle $ convertInputs inputs

convertInputs :: NonEmpty Input -> NonEmpty P.Input
convertInputs = NE.map convertInput

convertInput :: Input -> P.Input
convertInput (Input {comment, answers, input}) = do
  let (p1, p2) = convertAnswers answers
  P.Input p1 p2 Nothing comment input

convertAnswers :: Answers -> (P.Answer, P.Answer)
convertAnswers (Answers {p1, p2}) = (convertAnswer p1, convertAnswer p2)

convertAnswer :: Answer -> P.Answer
convertAnswer Unanswered = P.Unanswered
convertAnswer NilAnswer = P.NilAnswer
convertAnswer (Answer i) = P.Answer i
