module Y25Spec (spec) where

import Test.Hspec (Spec, parallel)
import TestUtils (testDay)
import Year.Y25.Day01 (day01)

spec :: Spec
spec = parallel $ do
  testDay day01
