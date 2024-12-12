module Puzzle.Writer
  ( writePuzzle,
    writeInput,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.IO.Utf8 qualified as TIO
import Puzzle.Types (Answer (..), Input (..), Puzzle (..))
import System.IO (Handle, IOMode (WriteMode), hClose, openFile)
import System.OsPath (OsPath, decodeUtf)

writePuzzle :: Puzzle -> OsPath -> IO ()
writePuzzle (Puzzle {puzzles}) p = do
  path <- decodeUtf p
  file <- openFile path WriteMode
  mapM_ (`writeInput` file) puzzles
  hClose file

  -- hack for whitespace
  written <- TIO.readFile path
  TIO.writeFile path (T.stripStart written)
  pure ()

writeInput :: Input -> Handle -> IO ()
writeInput i handle = do
  TIO.hPutStr handle "\n"
  TIO.hPutStr handle "#{"
  let parts = filter (not . T.null) [writeName i, writeP1 i, writeP2 i, writeComment i]
  TIO.hPutStr handle $ T.intercalate ", " parts
  TIO.hPutStrLn handle "}"
  TIO.hPutStrLn handle (input i)
  pure ()

writeName :: Input -> Text
writeName (Input {name}) = writeText name "name"

writeComment :: Input -> Text
writeComment (Input {comment}) = writeText comment "comment"

writeText :: Maybe Text -> Text -> Text
writeText (Just t) k = k <> "= \"" <> t <> "\""
writeText Nothing _ = ""

writeAnswer :: Answer -> Text -> Text
writeAnswer Unanswered _ = ""
writeAnswer NilAnswer p = p <> " = nil"
writeAnswer (IntAnswer a) p = p <> " = " <> display a

writeP1 :: Input -> Text
writeP1 (Input {answer1}) = writeAnswer answer1 "p1"

writeP2 :: Input -> Text
writeP2 (Input {answer2}) = writeAnswer answer2 "p2"
