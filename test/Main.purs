module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.CodePoints (testCodePoints)
import Test.CodeUnits (testCodeUnits)
import Test.BasicSpecs (runTestCases)
import Test.StackSafeCombinators (testStackSafeCombinators)

main :: Effect Unit
main = do
  log "Running basic spec test cases\n"
  runTestCases

  log "\n\nTesting CodeUnit parsing\n"
  testCodeUnits

  log "\n\nTesting CodePoint parsing\n"
  testCodePoints

  log "\n\nTesting stack safety of combinators\n"
  testStackSafeCombinators
