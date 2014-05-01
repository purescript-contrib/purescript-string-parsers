module Main where

import Data.Array
import Data.Maybe
import Data.Either

import Debug.Trace

import Control.Monad.Eff

import Text.Parsing.StringParser 

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
