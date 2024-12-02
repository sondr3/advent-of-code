{-# LANGUAGE FlexibleContexts #-}

module Y24Spec (spec) where

import Test.Hspec (Spec, parallel)
import TestUtils (testDay)
import Year.Y24.Day01 (day01)
import Year.Y24.Day02 (day02)

spec :: Spec
spec = parallel $ do
  testDay day01
  testDay day02
