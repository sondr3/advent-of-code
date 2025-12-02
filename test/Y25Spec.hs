module Y25Spec (spec) where

import Test.Hspec (Spec, parallel)
import TestUtils (testDay)
import Year.Y25.Day01 (day01)
import Year.Y25.Day02 (day02)

spec :: Spec
spec = parallel $ do
  testDay day01
  testDay day02
