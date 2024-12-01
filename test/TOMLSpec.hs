module TOMLSpec (spec) where

import TOML
import Test.Hspec (Spec, it, parallel)
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

spec :: Spec
spec = parallel $ do
  it "should parse titles" $ do
    parse parseTitle "" "title = \"Day 01\"" `shouldParse` "Day 01"
    parse parseTitle "" "title = \"day 123 OTHER\"" `shouldParse` "day 123 OTHER"
  it "should parse answers" $ do
    parse parseAnswers "" "{ p1 = 123, p2 = 456 }" `shouldParse` Answer (Just 123) (Just 456)
    parse parseAnswers "" "{p1 =123, p2= 456}" `shouldParse` Answer (Just 123) (Just 456)
    parse parseAnswers "" "{p1=5360891,p2=0}" `shouldParse` Answer (Just 5360891) (Just 0)
    parse parseAnswers "" "{p1=9876}" `shouldParse` Answer (Just 9876) Nothing
    parse parseAnswers "" "{p2=5432}" `shouldParse` Answer Nothing (Just 5432)
  it "should parse inputs" $ do
    parse parseDocumentInput "" "[[input]]\nanswers = { p1 = 123, p2 = 456 }\ninput = \"\"\"input\"\"\"" `shouldParse` Input Nothing (Answer (Just 123) (Just 456)) "input"
    parse parseDocumentInput "" "[[input]]\ncomment = \"some comment\"\nanswers = {p1=1, p2= 412}\ninput = \"\"\"input\nwith a bunch\nof\nnewlines\"\"\"" `shouldParse` Input (Just "some comment") (Answer (Just 1) (Just 412)) "input\nwith a bunch\nof\nnewlines"
