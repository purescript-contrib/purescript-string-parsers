module Test.Main where

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (isLeft, isRight, Either(..))
import Data.Functor (($>))
import Data.List (List(Nil), (:))
import Data.String (joinWith, singleton)
import Data.Unfoldable (replicate)
import Test.Assert (assert', ASSERT, assert)
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.Combinators (many1, endBy1, sepBy1, optionMaybe, many, chainl, fix, between)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.StringParser.String (anyDigit, eof, string, anyChar)
import Prelude hiding (between)

parens :: forall a. Parser a -> Parser a
parens = between (string "(") (string ")")

nested :: Parser Int
nested = fix $ \p -> (do
  string "a"
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
tryTest = try ((<>) <$> string "aa" <*> string "bb") <|>
          (<>) <$> string "aa" <*> string "cc"

canParse :: forall a. Parser a -> String -> Boolean
canParse p input = isRight $ runParser p input

parseFail :: forall a. Parser a -> String -> Boolean
parseFail p input = isLeft $ runParser p input

expectResult :: forall a. (Eq a) => a -> Parser a -> String -> Boolean
expectResult res p input = runParser p input == Right res

main :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
main = do
  assert' "many should not blow the stack" $ canParse (many (string "a")) (joinWith "" $ replicate 100000 "a")
  assert' "many failing after" $ parseFail (do
    as <- many (string "a")
    eof
    pure as) (joinWith "" (replicate 100000 "a") <> "b" )

  assert $ expectResult 3 nested "(((a)))"
  assert $ expectResult ("a":"a":"a":Nil)  (many (string "a")) "aaa"
  assert $ parseFail (many1 (string "a")) ""
  assert $ canParse (parens (do
    string "a"
    optionMaybe $ string "b")) "(ab)"
  assert $ expectResult ("a":"a":"a":Nil) (string "a" `sepBy1` string ",") "a,a,a"
  assert $ canParse (do
    as <- string "a" `endBy1` string ","
    eof
    pure as) "a,a,a,"
  assert' "opTest" $ expectResult "abc" opTest "a+b+c"
  assert' "exprTest" $ expectResult (-3) exprTest "1*2+3/4-5"
  assert' "tryTest "$ canParse tryTest "aacc"
  assert $ expectResult ('0':'1':'2':'3':'4':Nil) (many1 anyDigit) "01234/"
  assert $ expectResult ('5':'6':'7':'8':'9':Nil) (many1 anyDigit) "56789:"
