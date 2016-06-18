module Test.Main where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.String (singleton)
import Text.Parsing.StringParser (Parser, ParseError(..), try, runParser)
import Text.Parsing.StringParser.Combinators (many1, endBy1, sepBy1, optionMaybe, many, chainl, fix, between)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.StringParser.String (anyDigit, eof, string, anyChar, anyLetter, alphaNum)

parens :: forall a. Parser a -> Parser a
parens = between (string "(") (string ")")

nested :: Parser Int
nested = fix $ \p -> (do
  string "a"
  pure 0) <|> ((+) 1) <$> parens p

parseTest :: forall a eff. Show a => Parser a -> String -> Eff (console :: CONSOLE | eff) Unit
parseTest p input =
  case runParser p input of
    Left (ParseError err) -> logShow err
    Right result -> logShow result

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

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  parseTest nested "(((a)))"
  parseTest (many (string "a")) "aaa"
  parseTest (parens (do
    string "a"
    optionMaybe $ string "b")) "(ab)"
  parseTest (string "a" `sepBy1` string ",") "a,a,a"
  parseTest (do
    as <- string "a" `endBy1` string ","
    eof
    pure as) "a,a,a,"
  parseTest opTest "a+b+c"
  parseTest exprTest "1*2+3/4-5"
  parseTest tryTest "aacc"
  parseTest (many1 anyDigit) "01234/"
  parseTest (many1 anyDigit) "56789:"
  parseTest (many anyLetter) "aB"
  parseTest (many alphaNum) "aB3"
