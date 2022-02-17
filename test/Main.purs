module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.CodePoints (testCodePoints)
import Test.CodeUnits (testCodeUnits)

main :: Effect Unit
main = do

  log "Testing CodeUnit parsing\n"
  testCodeUnits

  log "\n\nTesting CodePoint parsing\n"
  testCodePoints
