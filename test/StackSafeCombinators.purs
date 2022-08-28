module Test.StackSafeCombinators where

import Prelude

import Data.Array (replicate)
import Data.Either (isRight)
import Data.String.Common as SC
import Effect (Effect)
import Effect.Class.Console (log)
import StringParser (Parser, char, runParser, sepEndBy)
import Test.Assert (assert)

canParse :: forall a. Parser a -> String -> Boolean
canParse p input = isRight $ runParser p input

testStackSafeCombinators :: Effect Unit
testStackSafeCombinators = do
  log "Running overflow tests"
  assert $ canParse (sepEndBy (char 'a') (char ';')) (SC.joinWith ";" $ replicate 100000 "a")
