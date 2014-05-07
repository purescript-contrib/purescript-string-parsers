module Main where

import Data.Array
import Data.Maybe
import Data.Either

import Debug.Trace

import Control.Monad.Eff

import Text.Parsing.StringParser 
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String
import Text.Parsing.StringParser.Expr

import qualified Test.QuickCheck as QC

parens :: forall a. Parser a -> Parser a
parens = between (string "(") (string ")")

nested :: Parser Number
nested = fix $ \p -> (do 
  string "a"
  return 0) <|> ((+) 1) <$> parens p

parseTest :: forall a eff. (Show a) => Parser a -> String -> Eff (trace :: Trace | eff) {}
parseTest p input = case runParser p input of
  Left (ParseError err) -> print err
  Right result -> print result

opTest :: Parser String
opTest = chainl anyChar (do 
  string "+"
  return (++)) ""
  
digit :: Parser Number
digit = (string "0" >>= \_ -> return 0) 
        <|> (string "1" >>= \_ -> return 1) 
        <|> (string "2" >>= \_ -> return 2) 
        <|> (string "3" >>= \_ -> return 3) 
        <|> (string "4" >>= \_ -> return 4) 
        <|> (string "5" >>= \_ -> return 5) 
        <|> (string "6" >>= \_ -> return 6) 
        <|> (string "7" >>= \_ -> return 7) 
        <|> (string "8" >>= \_ -> return 8) 
        <|> (string "9" >>= \_ -> return 9) 

exprTest :: Parser Number
exprTest = buildExprParser [[Infix (string "/" >>= \_ -> return (/)) AssocRight]
                           ,[Infix (string "*" >>= \_ -> return (*)) AssocRight]
                           ,[Infix (string "-" >>= \_ -> return (-)) AssocRight]
                           ,[Infix (string "+" >>= \_ -> return (+)) AssocRight]] digit

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
    return as) "a,a,a,"  
  parseTest opTest "a+b+c"
  parseTest exprTest "1*2+3/4-5"
