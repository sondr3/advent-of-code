module PuzzleParseSpec (spec) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Puzzle.Parser
import Puzzle.Types (Answer (..), Input (..), Puzzle (..))
import Test.Hspec (Spec, it, parallel)
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

spec :: Spec
spec = parallel $ do
  it "should parse simple headers" $ do
    parse parseHeader "" "#{p1 = 2, p2 = 3}" `shouldParse` (IntAnswer 2, IntAnswer 3, Nothing, Nothing)
    parse parseHeader "" "#{ p1 = 123, p2 = 456 }" `shouldParse` (IntAnswer 123, IntAnswer 456, Nothing, Nothing)
    parse parseHeader "" "#{p1 =123, p2= 456}" `shouldParse` (IntAnswer 123, IntAnswer 456, Nothing, Nothing)
    parse parseHeader "" "#{p1=5360891,p2=0}" `shouldParse` (IntAnswer 5360891, IntAnswer 0, Nothing, Nothing)
    parse parseHeader "" "#{p1=9876}" `shouldParse` (IntAnswer 9876, Unanswered, Nothing, Nothing)
    parse parseHeader "" "#{p2=5432}" `shouldParse` (Unanswered, IntAnswer 5432, Nothing, Nothing)
    parse parseHeader "" "#{p1=nil}" `shouldParse` (NilAnswer, Unanswered, Nothing, Nothing)
    parse parseHeader "" "#{p1= 123, p2 =nil}" `shouldParse` (IntAnswer 123, NilAnswer, Nothing, Nothing)
  it "should parse with title" $ do
    parse parseHeader "" "#{name=\"name\", p1=nil}" `shouldParse` (NilAnswer, Unanswered, Just "name", Nothing)
    parse parseHeader "" "#{name =   \"opa\" ,p2=5432}" `shouldParse` (Unanswered, IntAnswer 5432, Just "opa", Nothing)
  it "should parse puzzle inputs" $ do
    parse parsePuzzle "" "#{p1 = 1, p2 = 2}\n123\n123\n123\n\n#{p1=1}\n123\n123\n123\n"
      `shouldParse` Puzzle
        { puzzles =
            Input
              { answer1 = IntAnswer 1,
                answer2 = IntAnswer 2,
                comment = Nothing,
                name = Nothing,
                input = "123\n123\n123"
              }
              :| [ Input
                     { answer1 = IntAnswer 1,
                       answer2 = Unanswered,
                       comment = Nothing,
                       name = Nothing,
                       input = "123\n123\n123"
                     }
                 ]
        }
