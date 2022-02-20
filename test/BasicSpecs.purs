module Test.BasicSpecs where

import Prelude hiding (between)

import Test.Utils (AnyParser(..), mkAnyParser)
import Control.Alt ((<|>))
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Either (isRight)
import Data.List (List)
import Data.List as List
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Assert (assert')
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.CodePoints (anyChar, anyDigit, anyLetter, char, eof, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (between, chainl, chainl1, endBy, endBy1, lookAhead, many, many1, manyTill, sepBy, sepBy1, sepEndBy, sepEndBy1)

type TestInputs = { successes :: Array String, failures :: Array String }
type TestCase = { name :: String, parser :: AnyParser, inputs :: TestInputs }

testCases :: Array TestCase
testCases =
  [ { name: "anyChar"
    , parser: mkAnyParser anyChar
    -- TODO: test "🙂" which should fail
    -- this is an open upstream issue https://github.com/purescript/purescript-strings/issues/153
    , inputs: { successes: [ "a", "%" ], failures: [ "" ] }
    }
  , { name: "anyLetter"
    , parser: mkAnyParser anyLetter
    , inputs: { successes: [ "a" ], failures: [ "9" ] }
    }
  , { name: "skipSpaces"
    , parser: mkAnyParser $ skipSpaces *> anyChar
    , inputs: { successes: [ "  9", "9" ], failures: [ "9 " ] }
    }
  , { name: "map"
    , parser: mkAnyParser $ anyChar <#> const 3
    , inputs: { successes: [ "9" ], failures: [ "" ] }
    }
  , { name: "applicative"
    , parser: mkAnyParser $ (anyLetter <#> (\c -> (\c2 -> [ c, c2 ]))) <*> anyDigit
    , inputs: { successes: [ "a9" ], failures: [ "", "-", "a", "9" ] }
    }
  , { name: "alt"
    , parser: mkAnyParser $ anyLetter <|> anyDigit
    , inputs: { successes: [ "x", "9" ], failures: [ "", "-", "aa" ] }
    }
  , { name: "bind"
    , parser: mkAnyParser $ anyLetter >>= \letter -> char letter
    , inputs: { successes: [ "xx" ], failures: [ "", "-", "a", "aaa" ] }
    }
  , { name: "try"
    , parser: mkAnyParser $ try (anyLetter *> anyDigit) <|> char 'a'
    , inputs: { successes: [ "b9", "a6", "a" ], failures: [ "", "b", "-", "6" ] }
    }
  , { name: "lookAhead"
    , parser: mkAnyParser $ lookAhead (char 'a') *> anyLetter
    , inputs: { successes: [ "a" ], failures: [ "", "b" ] }
    }
  , { name: "many"
    , parser: mkAnyParser $ many (char 'a')
    , inputs: { successes: [ "", "a", "aaaa" ], failures: [ "b" ] }
    }
  , { name: "many1"
    , parser: mkAnyParser $ many1 (char 'a')
    , inputs: { successes: [ "a", "aaaa" ], failures: [ "", "b" ] }
    }
  , { name: "between"
    , parser: mkAnyParser $ between (char 'a') (char 'b') (char 'x')
    , inputs: { successes: [ "axb" ], failures: [ "", "x", "a", "b", "ab" ] }
    }
  , { name: "sepBy"
    , parser: mkAnyParser $ sepBy anyLetter (char ';')
    , inputs: { successes: [ "", "a", "a;b", "a;b;c" ], failures: [ ";", ";a", "a;", "ab", "a;ab" ] }
    }
  , { name: "sepBy1"
    , parser: mkAnyParser $ sepBy1 anyLetter (char ';')
    , inputs: { successes: [ "a", "a;b", "a;b;c" ], failures: [ "", ";", ";a", "a;", "ab", "a;ab" ] }
    }
  , { name: "sepEndBy"
    , parser: mkAnyParser $ sepEndBy anyLetter (char ';')
    -- TODO: ";" should be parsed successfully
    , inputs: { successes: [ "", "a", "a;b", "a;b;c", "a;" ], failures: [ ";", ";a", "ab", "a;ab" ] }
    }
  , { name: "sepEndBy1"
    , parser: mkAnyParser $ sepEndBy1 anyLetter (char ';')
    , inputs: { successes: [ "a", "a;b", "a;b;c", "a;" ], failures: [ "", ";", ";a", "ab", "a;ab" ] }
    }
  , { name: "endBy"
    , parser: mkAnyParser $ endBy anyLetter (char ';')
    -- TODO: ";" should be parsed successfully
    , inputs: { successes: [ "", "a;", "a;b;", "a;b;c;" ], failures: [ "a", ";", ";a", "ab", "a;b", "a;b;c" ] }
    }
  , { name: "endBy1"
    , parser: mkAnyParser $ endBy1 anyLetter (char ';')
    , inputs: { successes: [ "a;", "a;b;", "a;b;c;" ], failures: [ "", ";", "a", ";a", "ab", "a;b", "a;b;c" ] }
    }
  , { name: "manyTill"
    , parser: mkAnyParser $ manyTill anyLetter (char ';')
    , inputs: { successes: [ ";", "a;", "abc;" ], failures: [ "", "a", ";a", "ab", "a;b", "a;b;c" ] }
    }
  , { name: "manyTill overlapping"
    , parser: mkAnyParser $ manyTill anyLetter (char 'z')
    , inputs: { successes: [ "z", "az", "abcz" ], failures: [ "", "a", "za", "ab", "azb", "azbzc" ] }
    }
  , { name: "chainl"
    , parser: mkAnyParser $ chainl (string "x") (char '+' $> (<>)) ""
    , inputs: { successes: [ "", "x", "x+x+x+x" ], failures: [ "+", "+x", "x+", "x+x+", "xx", "xx+" ] }
    }
  , { name: "chainl1"
    , parser: mkAnyParser $ chainl1 (string "x") (char '+' $> (<>))
    , inputs: { successes: [ "x", "x+x+x+x" ], failures: [ "", "+", "+x", "x+", "x+x+", "xx", "xx+" ] }
    }
  ]

type TestResult = Writer (List String) Unit

reportError :: String -> TestResult
reportError = tell <<< List.singleton

runTestCases :: Effect Unit
runTestCases = do
  let errors = execWriter $ traverse evalTestCase testCases
  when (List.length errors > 0) do
    _ <- traverse log errors
    assert' "Errors found" false

evalTestCase :: TestCase -> TestResult
evalTestCase tc = do
  _ <- traverse assertSuccess tc.inputs.successes
  _ <- traverse assertFailure tc.inputs.failures
  pure unit
  where
  assertSuccess input =
    when (not (evalAnyParser tc.parser input)) do
      reportError ("Expected " <> tc.name <> " to succeed on '" <> input <> "' but it failed")
  assertFailure input =
    when (evalAnyParser tc.parser input) do
      reportError ("Expected " <> tc.name <> " to fail on '" <> input <> "' but it succeeded")

evalAnyParser :: AnyParser -> String -> Boolean
evalAnyParser (AnyParser anyP) input = anyP canFullyParse input

canFullyParse :: forall a. Parser a -> String -> Boolean
canFullyParse p input = isRight $ runParser (p *> eof) input