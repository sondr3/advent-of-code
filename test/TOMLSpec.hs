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
    parse parseTitle "" "title = \"day 123 OTHER\"" `shouldParse` "day 123 OTHER"
  it "should parse answers" $ do
    parse parseAnswers "" "{ p1 = 123, p2 = 456 }" `shouldParse` Answer 123 456
    parse parseAnswers "" "{p1 =123, p2= 456}" `shouldParse` Answer 123 456
    parse parseAnswers "" "{p1=5360891,p2=0}" `shouldParse` Answer 5360891 0
  it "should parse inputs" $ do
    parse parseInput "" "[[input]]\nanswers = { p1 = 123, p2 = 456 }\ninput = \"\"\"input\"\"\"" `shouldParse` Input Nothing (Answer 123 456) "input"
    parse parseInput "" "[[input]]\ncomment = \"some comment\"\nanswers = {p1=1, p2= 412}\ninput = \"\"\"input\nwith a bunch\nof\nnewlines\"\"\"" `shouldParse` Input (Just "some comment") (Answer 1 412) "input\nwith a bunch\nof\nnewlines"
