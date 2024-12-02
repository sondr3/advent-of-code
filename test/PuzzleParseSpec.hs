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
    parse parseHeader "" "#{p1 = 2, p2 = 3}" `shouldParse` (Answer 2, Answer 3, Nothing, Nothing)
    parse parseHeader "" "#{ p1 = 123, p2 = 456 }" `shouldParse` (Answer 123, Answer 456, Nothing, Nothing)
    parse parseHeader "" "#{p1 =123, p2= 456}" `shouldParse` (Answer 123, Answer 456, Nothing, Nothing)
    parse parseHeader "" "#{p1=5360891,p2=0}" `shouldParse` (Answer 5360891, Answer 0, Nothing, Nothing)
    parse parseHeader "" "#{p1=9876}" `shouldParse` (Answer 9876, Unanswered, Nothing, Nothing)
    parse parseHeader "" "#{p2=5432}" `shouldParse` (Unanswered, Answer 5432, Nothing, Nothing)
    parse parseHeader "" "#{p1=nil}" `shouldParse` (NilAnswer, Unanswered, Nothing, Nothing)
    parse parseHeader "" "#{p1= 123, p2 =nil}" `shouldParse` (Answer 123, NilAnswer, Nothing, Nothing)
  it "should parse with title" $ do
    parse parseHeader "" "#{name=\"name\", p1=nil}" `shouldParse` (NilAnswer, Unanswered, Just "name", Nothing)
    parse parseHeader "" "#{name =   \"opa\" ,p2=5432}" `shouldParse` (Unanswered, Answer 5432, Just "opa", Nothing)
  it "should parse puzzle inputs" $ do
    parse parsePuzzle "" "#{p1 = 1, p2 = 2}\n123\n123\n123\n\n#{p1=1}\n123\n123\n123\n"
      `shouldParse` Puzzle
        { inputs =
            Input
              { part1 = Answer 1,
                part2 = Answer 2,
                comment = Nothing,
                name = Nothing,
                input = "123\n123\n123\n"
              }
              :| [ Input
                     { part1 = Answer 1,
                       part2 = Unanswered,
                       comment = Nothing,
                       name = Nothing,
                       input = "123\n123\n123\n"
                     }
                 ]
        }
