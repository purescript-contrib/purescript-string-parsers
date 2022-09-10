module Test.BasicSpecs where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Either (Either(..), isLeft, isRight)
import Data.List (List)
import Data.List as List
import Data.Maybe (fromJust)
import Data.String (CodePoint, codePointAt)
import Data.String.CodePoints (drop)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)
import StringParser (Parser, anyChar, anyCodePoint, anyDigit, anyLetter, between, chainl, chainl1, char, codePoint, endBy, endBy1, eof, lookAhead, many, many1, many1Till, manyTill, optionMaybe, runParser, sepBy, sepBy1, sepEndBy, sepEndBy1, skipSpaces, string, try, tryAhead)
import Test.Assert (assert')
import Test.Utils (AnyParser(..), mkAnyParser)

data PartialSuccessTestCase = P String String -- input unconsumed
type TestCases =
  { successes :: Array String -- Assert string fully consumed and parser returned success
  , failures :: Array String -- Assert nothing consumed and parser returned failure
  , partials :: Array PartialSuccessTestCase -- Assert unconsumed substring and parser returned failure
  }

type ParserSpec = { name :: String, parser :: AnyParser, inputs :: TestCases }

codePointLiteral :: String -> CodePoint
codePointLiteral s = unsafePartial $ fromJust $ codePointAt 0 s

testCases :: Array ParserSpec
testCases =
  [ { name: "anyChar"
    , parser: mkAnyParser anyChar
    , inputs: { successes: [ "a", "%" ], failures: [ "", "🙂" ], partials: [ P "aa" "a" ] }
    }
  , { name: "many anyChar"
    , parser: mkAnyParser $ many anyChar
    , inputs: { successes: [ "", "a", "%", "aa" ], failures: [ "🙂" ], partials: [] }
    }
  , { name: "anyCodePoint"
    , parser: mkAnyParser anyCodePoint
    , inputs: { successes: [ "a", "%", "🙂" ], failures: [ "" ], partials: [ P "aa" "a" ] }
    }
  , { name: "codePoint"
    , parser: mkAnyParser $ codePoint $ codePointLiteral "🙂"
    , inputs: { successes: [ "🙂" ], failures: [ "", "a", "aa" ], partials: [] }
    }
  , { name: "anyLetter"
    , parser: mkAnyParser anyLetter
    , inputs: { successes: [ "a" ], failures: [ "9" ], partials: [ P "aa" "a" ] }
    }
  , { name: "skipSpaces"
    , parser: mkAnyParser $ skipSpaces *> anyChar
    , inputs: { successes: [ "  9", "9" ], failures: [], partials: [ P "9 " " " ] }
    }
  , { name: "map"
    , parser: mkAnyParser $ anyChar <#> const 3
    , inputs: { successes: [ "9" ], failures: [ "" ], partials: [] }
    }
  , { name: "applicative"
    , parser: mkAnyParser $ (anyLetter <#> (\c -> (\c2 -> [ c, c2 ]))) <*> anyDigit
    , inputs: { successes: [ "a9" ], failures: [ "", "-", "a", "9" ], partials: [] }
    }
  , { name: "alt"
    , parser: mkAnyParser $ anyLetter <|> anyDigit
    , inputs: { successes: [ "x", "9" ], failures: [ "", "-" ], partials: [ P "aa" "a", P "a6" "6" ] }
    }
  , { name: "bind"
    , parser: mkAnyParser $ anyLetter >>= \letter -> char letter
    , inputs: { successes: [ "xx" ], failures: [ "", "-", "a" ], partials: [ P "aaa" "a" ] }
    }
  , { name: "try"
    , parser: mkAnyParser $ try (anyLetter *> anyDigit) <|> char 'a'
    , inputs: { successes: [ "b9", "a6", "a" ], failures: [ "", "b", "-", "6" ], partials: [] }
    }
  , { name: "lookAhead"
    , parser: mkAnyParser $ lookAhead (char 'a') *> anyLetter
    , inputs: { successes: [ "a" ], failures: [ "", "b" ], partials: [ P "ab" "b" ] }
    }
  , { name: "tryAhead"
    , parser: mkAnyParser $ tryAhead (char 'a' *> anyDigit) *> (anyChar *> anyChar) <|> (anyDigit *> anyDigit)
    , inputs: { successes: [ "a6", "66" ], failures: [ "", "b", "-", "6", "aa", "aa66" ], partials: [ P "a6aa" "aa" ] }
    }
  , { name: "many"
    , parser: mkAnyParser $ many (char 'a')
    , inputs: { successes: [ "", "a", "aaaa" ], failures: [ "b" ], partials: [] }
    }
  , { name: "many no consumption"
    , parser: mkAnyParser $ many (eof)
    , inputs: { successes: [ "" ], failures: [ "b" ], partials: [] }
    }
  , { name: "many1"
    , parser: mkAnyParser $ many1 (char 'a')
    , inputs: { successes: [ "a", "aaaa" ], failures: [ "", "b" ], partials: [] }
    }
  , { name: "many1 no consumption"
    , parser: mkAnyParser $ many1 (eof)
    , inputs: { successes: [ "" ], failures: [ "b" ], partials: [] }
    }
  , { name: "manyTill"
    , parser: mkAnyParser $ manyTill anyLetter (char ';')
    , inputs: { successes: [ ";", "a;", "abc;" ], failures: [ "", "a", "ab" ], partials: [ P ";a" "a", P "a;b" "b", P "a;b;c" "b;c" ] }
    }
  , { name: "manyTill no consumption"
    , parser: mkAnyParser $ manyTill (optionMaybe (char 'a')) (char ';')
    , inputs: { successes: [ ";", "a;", "aaa;" ], failures: [ "", "a", "aa" ], partials: [ P ";a" "a", P "a;a" "a", P "a;a;a" "a;a" ] }
    }
  , { name: "manyTill overlapping"
    , parser: mkAnyParser $ manyTill anyLetter (char 'z')
    , inputs: { successes: [ "z", "az", "abcz" ], failures: [ "", "a", "za", "ab" ], partials: [ P "azb" "b", P "azbzc" "bzc" ] }
    }
  , { name: "many1Till"
    , parser: mkAnyParser $ many1Till anyLetter (char ';')
    , inputs: { successes: [ "a;", "abc;" ], failures: [ "", ";", "a", ";a", "ab" ], partials: [ P "a;b" "b", P "a;b;c" "b;c" ] }
    }
  , { name: "many1Till overlapping"
    , parser: mkAnyParser $ many1Till anyLetter (char 'z')
    , inputs: { successes: [ "az", "abcz" ], failures: [ "", "z", "a", "za", "ab" ], partials: [ P "azb" "b", P "azbzc" "bzc" ] }
    }
  , { name: "between"
    , parser: mkAnyParser $ between (char 'a') (char 'b') (char 'x')
    , inputs: { successes: [ "axb" ], failures: [ "", "x", "a", "b", "ab" ], partials: [] }
    }
  , { name: "sepBy"
    , parser: mkAnyParser $ sepBy anyLetter (char ';')
    , inputs: { successes: [ "", "a", "a;b", "a;b;c" ], failures: [ ";", ";a", "a;", "ab", "a;ab" ], partials: [] }
    }
  , { name: "sepBy1"
    , parser: mkAnyParser $ sepBy1 anyLetter (char ';')
    , inputs: { successes: [ "a", "a;b", "a;b;c" ], failures: [ "", ";", ";a", "a;", "ab", "a;ab" ], partials: [] }
    }
  , { name: "sepEndBy"
    , parser: mkAnyParser $ sepEndBy anyLetter (char ';')
    , inputs: { successes: [ "", ";", "a", "a;b", "a;b;c", "a;" ], failures: [ ";a", "ab", "a;ab" ], partials: [] }
    }
  , { name: "sepEndBy1"
    , parser: mkAnyParser $ sepEndBy1 anyLetter (char ';')
    , inputs: { successes: [ "a", "a;b", "a;b;c", "a;" ], failures: [ "", ";", ";a", "ab", "a;ab" ], partials: [] }
    }
  , { name: "endBy"
    , parser: mkAnyParser $ endBy anyLetter (char ';')
    , inputs: { successes: [ ";", "a;", "a;b;", "a;b;c;" ], failures: [ "", "a", ";a", "ab", "a;b", "a;b;c" ], partials: [] }
    }
  , { name: "endBy1"
    , parser: mkAnyParser $ endBy1 anyLetter (char ';')
    , inputs: { successes: [ "a;", "a;b;", "a;b;c;" ], failures: [ "", ";", "a", ";a", "ab", "a;b", "a;b;c" ], partials: [] }
    }
  , { name: "chainl"
    , parser: mkAnyParser $ chainl (string "x") (char '+' $> (<>)) ""
    , inputs: { successes: [ "", "x", "x+x+x+x" ], failures: [ "+", "+x", "x+", "x+x+", "xx", "xx+" ], partials: [] }
    }
  , { name: "chainl1"
    , parser: mkAnyParser $ chainl1 (string "x") (char '+' $> (<>))
    , inputs: { successes: [ "x", "x+x+x+x" ], failures: [ "", "+", "+x", "x+", "x+x+", "xx", "xx+" ], partials: [] }
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

evalTestCase :: ParserSpec -> TestResult
evalTestCase tc = do
  _ <- traverse assertSuccess tc.inputs.successes
  _ <- traverse assertFailure tc.inputs.failures
  _ <- traverse assertPartial tc.inputs.partials
  pure unit
  where
  assertSuccess input =
    when (isLeft (evalAnyParser tc.parser input)) do
      reportError ("Expected " <> tc.name <> " to succeed on  " <> input <> "  but it failed")
  assertFailure input = assertPartial (P input input)
  assertPartial (P input expectedUnconsumed) =
    case evalAnyParser tc.parser input of
      Left unconsumed -> when (unconsumed /= expectedUnconsumed) do
        reportError ("Expected " <> tc.name <> " to fail on  " <> input <> "  and consume " <> showUnconsumed expectedUnconsumed <> " but it consumed " <> showUnconsumed unconsumed)
        where
        showUnconsumed s = if s == "" then "everything" else if s == input then "nothing" else "until  " <> s <> " "
      Right _ ->
        reportError ("Expected " <> tc.name <> " to fail on  " <> input <> "  but it succeeded")

evalAnyParser :: AnyParser -> String -> Either String Unit
evalAnyParser (AnyParser anyP) input = anyP canFullyParse input

canFullyParse :: forall a. Parser a -> String -> Either String Unit
canFullyParse p input = case runParser (p *> eof) input of
  Left { pos } -> Left $ drop pos input
  Right _ -> Right unit
