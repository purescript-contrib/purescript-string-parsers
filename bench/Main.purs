-- | # Benchmarking
-- |
-- |     spago run --main Bench.Main
-- |
-- | This benchmark suite is intended to guide changes to this package so that
-- | we can compare the benchmarks of different commits.

module Bench.Main where

import Prelude

import Data.Array (fold, replicate)
import Data.List (manyRec)
import Data.List.Types (List)
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (benchWith)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodePoints as StringParser.CodePoints
import Text.Parsing.StringParser.CodeUnits as StringParser.CodeUnits

string23 :: String
string23 = "23"

string23_2 :: String
string23_2 = fold $ replicate 2 string23

string23_10000 :: String
string23_10000 = fold $ replicate 10000 string23

parse23DigitPoints :: Parser (List Char)
parse23DigitPoints = manyRec StringParser.CodePoints.anyDigit

parse23DigitUnits :: Parser (List Char)
parse23DigitUnits = manyRec StringParser.CodeUnits.anyDigit

parse23StringPoints :: Parser (List String)
parse23StringPoints = manyRec $ StringParser.CodePoints.string "23"

parse23StringUnits :: Parser (List String)
parse23StringUnits = manyRec $ StringParser.CodeUnits.string "23"

parse23RegexPoints :: Parser (List String)
parse23RegexPoints = manyRec $ StringParser.CodePoints.regex """\d\d"""

parse23RegexUnits :: Parser (List String)
parse23RegexUnits = manyRec $ StringParser.CodeUnits.regex """\d\d"""

main :: Effect Unit
main = do
  -- log $ show $ runParser string23_2 parse23
  -- log $ show $ Regex.match pattern23 string23_2
  -- log $ show $ runParser stringSkidoo_2 parseSkidoo
  -- log $ show $ Regex.match patternSkidoo stringSkidoo_2
  log "StringParser.runParser parse23DigitPoints"
  benchWith 200
    $ \_ -> runParser parse23DigitPoints string23_10000
  log "StringParser.runParser parse23DigitUnits"
  benchWith 200
    $ \_ -> runParser parse23DigitUnits string23_10000

  log "StringParser.runParser parse23StringPoints"
  benchWith 200
    $ \_ -> runParser parse23StringPoints string23_10000
  log "StringParser.runParser parse23StringUnits"
  benchWith 200
    $ \_ -> runParser parse23StringUnits string23_10000

  log "StringParser.runParser parse23RegexPoints"
  benchWith 200
    $ \_ -> runParser parse23RegexPoints string23_10000
  log "StringParser.runParser parse23RegexUnits"
  benchWith 200
    $ \_ -> runParser parse23RegexUnits string23_10000
