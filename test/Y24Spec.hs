{-# LANGUAGE FlexibleContexts #-}

module Y24Spec (spec) where

import Test.Hspec (Spec, parallel)
import TestUtils (testDay)
import Year.Y24.Day01 (day01)

spec :: Spec
spec = parallel $ do
  testDay day01
