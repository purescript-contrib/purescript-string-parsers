module Text.Parsing.StringParser.String where

import Data.Maybe
import Data.String (charAt, fromChar, length, take, indexOf')
import Text.Parsing.StringParser

eof :: Parser Unit
eof = Parser (\s fc sc -> case s of
  { str = str, pos = i } | i < length str -> fc i (ParseError "Expected EOF")
  _ -> sc unit s)

anyChar :: Parser String
anyChar = Parser (\s fc sc -> case s of
  { str = str, pos = i } -> case charAt i str of
    Just chr -> sc (fromChar chr) { str: str, pos: i + 1 }
    Nothing -> fc i (ParseError "Unexpected EOF"))

string :: String -> Parser String
string nt = Parser (\s fc sc -> case s of
  { str = str, pos = i } | indexOf' nt i str == i -> sc nt { str: str, pos: i + length nt }
  { pos = i } -> fc i (ParseError $ "Expected '" ++ nt ++ "'."))
