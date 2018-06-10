module Test.Main where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Either (isLeft, isRight, Either(..))
import Data.Foldable (fold)
import Data.List (List(Nil), (:))
import Data.List.Lazy (take, repeat)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (singleton)
import Data.String.Common as SC
import Data.Unfoldable (replicate)
import Effect (Effect)
import Test.Assert (assert', assert)
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.Combinators (many1, endBy1, sepBy1, optionMaybe, many, manyTill, many1Till, chainl, fix, between)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.StringParser.String (anyDigit, eof, string, anyChar, regex)

parens :: forall a. Parser a -> Parser a
parens = between (string "(") (string ")")

nested :: Parser Int
nested = fix $ \p -> (do
  _ <- string "a"
  pure 0) <|> ((+) 1) <$> parens p

opTest :: Parser String
opTest = chainl (singleton <$> anyChar) (string "+" $> append) ""

digit :: Parser Int
digit = string "0" $> 0
    <|> string "1" $> 1
    <|> string "2" $> 2
    <|> string "3" $> 3
    <|> string "4" $> 4
    <|> string "5" $> 5
    <|> string "6" $> 6
    <|> string "7" $> 7
    <|> string "8" $> 8
    <|> string "9" $> 9

exprTest :: Parser Int
exprTest = buildExprParser [ [Infix (string "/" >>= \_ -> pure div) AssocRight]
                           , [Infix (string "*" >>= \_ -> pure mul) AssocRight]
                           , [Infix (string "-" >>= \_ -> pure sub) AssocRight]
                           , [Infix (string "+" >>= \_ -> pure add) AssocRight]
                           ] digit

tryTest :: Parser String
            -- reduce the possible array of matches to 0 or 1 elements to aid Array pattern matching
tryTest =
  try (string "aa" <> string "bb") <|>
      (string "aa" <> string "cc")

canParse :: forall a. Parser a -> String -> Boolean
canParse p input = isRight $ runParser p input

parseFail :: forall a. Parser a -> String -> Boolean
parseFail p input = isLeft $ runParser p input

expectResult :: forall a. (Eq a) => a -> Parser a -> String -> Boolean
expectResult res p input = runParser p input == Right res

main :: Effect Unit
main = do
  assert' "many should not blow the stack" $ canParse (many (string "a")) (SC.joinWith "" $ replicate 100000 "a")
  assert' "many failing after" $ parseFail (do
    as <- many (string "a")
    eof
    pure as) (SC.joinWith "" (replicate 100000 "a") <> "b" )

  assert $ expectResult 3 nested "(((a)))"
  assert $ expectResult ("a":"a":"a":Nil)  (many (string "a")) "aaa"
  assert $ parseFail (many1 (string "a")) ""
  assert $ canParse (parens (do
    _ <- string "a"
    optionMaybe $ string "b")) "(ab)"
  assert $ expectResult (NonEmptyList ("a" :| "a":"a":Nil)) (string "a" `sepBy1` string ",") "a,a,a"
  assert $ canParse (do
    as <- string "a" `endBy1` string ","
    eof
    pure as) "a,a,a,"
  assert' "opTest" $ expectResult "abc" opTest "a+b+c"
  assert' "exprTest" $ expectResult (-3) exprTest "1*2+3/4-5"
  assert' "tryTest "$ canParse tryTest "aacc"
  assert $ expectResult (NonEmptyList ('0' :| '1':'2':'3':'4':Nil)) (many1 anyDigit) "01234/"
  assert $ expectResult (NonEmptyList ('5' :| '6':'7':'8':'9':Nil)) (many1 anyDigit) "56789:"
  assert $ expectResult "aaaa" (regex "a+") "aaaab"
  assert $ expectResult ("a":"a":"a":Nil)  (manyTill (string "a") (string "b")) "aaab"
  assert $ expectResult Nil (manyTill (string "a") (string "b")) "b"
  assert $ expectResult (NonEmptyList ("a" :| "a":"a":Nil)) (many1Till (string "a") (string "b")) "aaab"
  assert $ parseFail (many1Till (string "a") (string "b")) "b"
  -- check against overflow
  assert $ canParse (many1Till (string "a") (string "and")) $ (fold <<< take 10000 $ repeat "a") <> "and"
  -- check correct order
  assert $ expectResult (NonEmptyList ('a' :| 'b':'c':Nil)) (many1Till anyChar (string "d")) "abcd"
