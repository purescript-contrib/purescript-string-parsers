module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.CodePoints (testCodePoints)
import Test.CodeUnits (testCodeUnits)

main :: Effect Unit
main = do
  log "Testing CodePoint parsing\n"
  testCodePoints

  log "\n\nTesting CodeUnit parsing\n"
  testCodeUnits
