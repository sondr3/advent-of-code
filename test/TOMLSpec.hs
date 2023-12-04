module TOMLSpec (spec) where

import TOML
import Test.Hspec (Spec, it, parallel)
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)
import Universum

spec :: Spec
spec = parallel $ do
  it "should parse titles" $ do
    parse parseTitle "" "title = \"Day 01\"" `shouldParse` "Day 01"
    parse parseTitle "" "title = \"day_123_OTHER\"" `shouldParse` "day_123_OTHER"
  it "should parse answers" $ do
    parse parseAnswers "" "{ p1 = 123, p2 = 456 }" `shouldParse` Answer 123 456
    parse parseAnswers "" "{p1 =123, p2= 456}" `shouldParse` Answer 123 456
    parse parseAnswers "" "{p1=5360891,p2=0}" `shouldParse` Answer 5360891 0
  it "should parse inputs" $ do
    parse parseInput "" "[p1]\nanswers = { p1 = 123, p2 = 456 }\ninput = \"\"\"input\"\"\"" `shouldParse` Input "p1" (Answer 123 456) "input"
    parse parseInput "" "[p1]\nanswers = {p1=1, p2= 412}\ninput = \"\"\"input\nwith a bunch\nof\nnewlines\"\"\"" `shouldParse` Input "p1" (Answer 1 412) "input\nwith a bunch\nof\nnewlines"
