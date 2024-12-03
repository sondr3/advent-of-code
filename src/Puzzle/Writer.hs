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
writePuzzle (Puzzle {inputs}) p = do
  path <- decodeUtf p
  file <- openFile path WriteMode
  mapM_ (`writeInput` file) inputs
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
writeName (Input {name}) = writeText name

writeComment :: Input -> Text
writeComment (Input {comment}) = writeText comment

writeText :: Maybe Text -> Text
writeText (Just t) = "name = \"" <> t <> "\""
writeText Nothing = ""

writeAnswer :: Answer -> Text -> Text
writeAnswer Unanswered _ = ""
writeAnswer NilAnswer p = p <> " = nil"
writeAnswer (Answer a) p = p <> " = " <> display a

writeP1 :: Input -> Text
writeP1 (Input {part1}) = writeAnswer part1 "p1"

writeP2 :: Input -> Text
writeP2 (Input {part2}) = writeAnswer part2 "p2"
