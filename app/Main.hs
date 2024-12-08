module Main (main) where

import AoC (Answer (..), Input (Input), Puzzle (..), padNum, writePuzzle)
import Data.Aeson (object, (.=))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Display.Core (Display)
import Data.Text.IO.Utf8 qualified as TIO
import Data.Text.Lazy.IO qualified as TLIO
import Options.Applicative
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.OsPath (encodeUtf)
import System.Process (callCommand)
import Text.Megaparsec (errorBundlePretty)
import Text.Mustache (compileMustacheText, renderMustache)

data NewOptions = NewOptions
  { year :: Text,
    day :: Text,
    skip :: Bool
  }

newtype Command
  = New NewOptions

data App = App {_optCommand :: Command, _verbose :: Bool}

writeTemplate :: NewOptions -> IO ()
writeTemplate NewOptions {..} = do
  f <- TIO.readFile "app/DayXX.stache"
  let yr = "Y" <> display year
      d = padNum $ read $ T.unpack day
      tmpl = compileMustacheText "day" f
  case tmpl of
    Left err -> putStrLn (errorBundlePretty err)
    Right t -> do
      let res = renderMustache t $ object ["day" .= day, "padDay" .= d, "year" .= year, "yy" .= yr]
          path = "src/Year/" <> yr <> "/" <> "Day" <> d <> ".hs"
      if skip
        then pure ()
        else do
          TLIO.writeFile (T.unpack path) res
          callCommand "cabal-gild -i aoc.cabal -o aoc.cabal"

readPuzzle :: NewOptions -> IO ()
readPuzzle NewOptions {..} = do
  inputs <- runInputT defaultSettings (loop [])
  path <- encodeUtf ("inputs/" <> "20" <> T.unpack year <> "/day" <> (T.unpack . padNum $ read . T.unpack $ day) <> ".aoc")
  writePuzzle (Puzzle (NE.fromList $ reverse inputs)) path
  where
    loop :: [Input Int] -> InputT IO [Input Int]
    loop xs = do
      name <- readLine "name: " T.pack
      if name == Just "done"
        then pure xs
        else do
          p1 <- readAnswer "p1: "
          p2 <- readAnswer "p2: "
          input <- readInput "input: "
          loop $ Input p1 p2 Nothing name input : xs

readAnswer :: (Eq a, Display a, Read a) => String -> InputT IO (Answer a)
readAnswer prompt = do
  cmd <- getInputLine prompt
  case cmd of
    Nothing -> pure Unanswered
    Just "" -> pure Unanswered
    Just "nil" -> pure NilAnswer
    Just ans -> pure $ Answer (read ans)

readLine :: String -> (String -> a) -> InputT IO (Maybe a)
readLine prompt f = do
  cmd <- getInputLine prompt
  case cmd of
    Nothing -> pure Nothing
    Just "" -> pure Nothing
    Just c -> pure $ Just (f c)

readInput :: Text -> InputT IO Text
readInput prompt = outputStrLn (T.unpack prompt) *> loop []
  where
    loop :: [Text] -> InputT IO Text
    loop acc = do
      minput <- getInputLine ""
      case minput of
        Nothing -> return $ T.unlines $ reverse acc
        Just line -> loop ((T.strip . T.pack) line : acc)

run :: App -> IO ()
run (App (New o@NewOptions {..}) _) = do
  if T.length year /= 2
    then do
      TIO.putStrLn "Years should be written as 24, 25 etc"
      pure ()
    else do
      writeTemplate o
      readPuzzle o

app :: Parser App
app = App <$> commands <*> debug
  where
    debug = switch (long "debug" <> short 'd' <> help "Print debug information")

commands :: Parser Command
commands = subparser newCmd
  where
    newCmd = command "new" (info newOptions (progDesc "Start a new day, a glorious day"))
    newOptions :: Parser Command
    newOptions = New <$> (NewOptions <$> y <*> d <*> s)
    y, d :: Parser Text
    y = strArgument (metavar "YEAR" <> help "Year")
    d = strArgument (metavar "DAY" <> help "Day")
    s = switch (long "skip" <> short 's' <> help "Skip IO")

main :: IO ()
main = do
  options <- execParser opts
  run options
  where
    opts :: ParserInfo App
    opts = info (app <**> helper) (fullDesc <> progDesc "Do some cool stuff" <> header "aoc - for dummies")
