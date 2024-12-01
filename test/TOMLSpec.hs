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
    parse parseAnswers "" "{ p1 = 123, p2 = 456 }" `shouldParse` Answers (Answer 123) (Answer 456)
    parse parseAnswers "" "{p1 =123, p2= 456}" `shouldParse` Answers (Answer 123) (Answer 456)
    parse parseAnswers "" "{p1=5360891,p2=0}" `shouldParse` Answers (Answer 5360891) (Answer 0)
    parse parseAnswers "" "{p1=9876}" `shouldParse` Answers (Answer 9876) Unanswered
    parse parseAnswers "" "{p2=5432}" `shouldParse` Answers Unanswered (Answer 5432)
    parse parseAnswers "" "{p1=nil}" `shouldParse` Answers NilAnswer Unanswered
    parse parseAnswers "" "{p1= 123, p2 =nil}" `shouldParse` Answers (Answer 123) NilAnswer
  it "should parse inputs" $ do
    parse parseDocumentInput "" "[[input]]\nanswers = { p1 = 123, p2 = 456 }\ninput = \"\"\"input\"\"\"" `shouldParse` Input Nothing (Answers (Answer 123) (Answer 456)) "input"
    parse parseDocumentInput "" "[[input]]\ncomment = \"some comment\"\nanswers = {p1=1, p2= 412}\ninput = \"\"\"input\nwith a bunch\nof\nnewlines\"\"\"" `shouldParse` Input (Just "some comment") (Answers (Answer 1) (Answer 412)) "input\nwith a bunch\nof\nnewlines"
