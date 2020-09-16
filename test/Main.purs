module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.CodeUnits (testCodeUnits)

main :: Effect Unit
main = do
  log "\n\nTesting CodeUnit parsing\n"
  testCodeUnits
