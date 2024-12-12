module Main (main) where

import AoC (Answer (..), Input (Input), Puzzle (..), padNum, writePuzzle)
import Data.Aeson (object, (.=))
import Data.List ((!?))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as TIO
import Data.Text.Lazy.IO qualified as TLIO
import Options.Applicative
import Solutions (benchmarkSolution, solveSolution)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment.Blank (withArgs)
import System.OsPath (encodeUtf)
import System.Process (callCommand)
import Text.Megaparsec (errorBundlePretty)
import Text.Mustache (compileMustacheText, renderMustache)
import Year (Year (..), longYear)
import Year.Y24 qualified
import Year.Y24 qualified as Year.Y23

data NewOptions = NewOptions
  { year :: Year,
    day :: Int,
    skip :: Bool
  }

data SolveOptions = SolveOptions
  { year :: Year,
    day :: Int
  }

data Command
  = New NewOptions
  | Solve SolveOptions

data App = App {_optCommand :: Command, _verbose :: Bool}

writeTemplate :: NewOptions -> IO ()
writeTemplate NewOptions {..} = do
  f <- TIO.readFile "app/DayXX.stache"
  case compileMustacheText "day" f of
    Left err -> putStrLn (errorBundlePretty err)
    Right t -> do
      let res = renderMustache t $ object ["day" .= day, "padDay" .= padNum day, "year" .= show year]
          path = "src/Year/" <> show year <> "/" <> "Day" <> T.unpack (padNum day) <> ".hs"
      if skip
        then pure ()
        else do
          TLIO.writeFile path res
          callCommand "cabal-gild -i aoc.cabal -o aoc.cabal"

readPuzzle :: NewOptions -> IO ()
readPuzzle NewOptions {..} = do
  inputs <- runInputT defaultSettings (loop [])
  path <- encodeUtf (T.unpack ("inputs/" <> longYear year <> "/day" <> padNum day <> ".aoc"))
  writePuzzle (Puzzle (NE.fromList $ reverse inputs)) path
  where
    loop :: [Input] -> InputT IO [Input]
    loop xs = do
      name <- readLine "name: " T.pack
      if name == Just "done"
        then pure xs
        else do
          p1 <- readAnswer "p1: "
          p2 <- readAnswer "p2: "
          input <- readInput "input: "
          loop $ Input p1 p2 Nothing name (T.strip input) : xs

readAnswer :: String -> InputT IO Answer
readAnswer prompt = do
  cmd <- getInputLine prompt
  case cmd of
    Nothing -> pure Unanswered
    Just "" -> pure Unanswered
    Just "nil" -> pure NilAnswer
    Just ans -> pure $ IntAnswer (read ans)

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

solveDay :: SolveOptions -> IO ()
solveDay SolveOptions {..} = do
  let sols = case year of
        Y23 -> Year.Y23.solutions
        Y24 -> Year.Y24.solutions
  _ <- solveSolution (sols !? (day - 1))
  _ <- withArgs [] $ benchmarkSolution (sols !? (day - 1))
  pure ()

run :: App -> IO ()
run (App (Solve o) _) = solveDay o
run (App (New o) _) = do
  writeTemplate o
  readPuzzle o

app :: Parser App
app = App <$> commands <*> debug
  where
    debug = switch (long "debug" <> short 'd' <> help "Print debug information")

commands :: Parser Command
commands = subparser (newCmd <> runCmd)
  where
    newCmd = command "new" (info newOptions (progDesc "Start a new day, a glorious day"))
    runCmd = command "solve" (info solveOptions (progDesc "Run a day, fingers crossed"))
    newOptions, solveOptions :: Parser Command
    newOptions = New <$> (NewOptions <$> y <*> d <*> s)
    solveOptions = Solve <$> (SolveOptions <$> y <*> d)
    y :: Parser Year
    y = argument auto (metavar "YEAR" <> help "Year")
    d :: Parser Int
    d = argument auto (metavar "DAY" <> help "Day")
    s = switch (long "skip" <> short 's' <> help "Skip IO")

main :: IO ()
main = do
  options <- execParser opts
  run options
  where
    opts :: ParserInfo App
    opts = info (app <**> helper) (fullDesc <> progDesc "Do some cool stuff" <> header "aoc - for dummies")
