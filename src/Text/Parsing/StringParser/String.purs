-- | Primitive parsers for strings.

module Text.Parsing.StringParser.String
  ( eof
  , anyChar
  , anyDigit
  , string
  ) where

import Prelude

import Data.Foldable (notElem)
import Data.Maybe (Maybe(..))
import Data.String (charAt, fromChar, length, indexOf')
import Text.Parsing.StringParser

import qualified Data.String.Regex as Rx

-- | Match the end of the file.
eof :: Parser Unit
eof = Parser (\s fc sc -> case s of
  { str = str, pos = i } | i < length str -> fc i (ParseError "Expected EOF")
  _ -> sc unit s)

-- | Match any character.
anyChar :: Parser Char
anyChar = Parser (\s fc sc -> case s of
  { str = str, pos = i } -> case charAt i str of
    Just chr -> sc chr { str: str, pos: i + 1 }
    Nothing -> fc i (ParseError "Unexpected EOF"))

-- | Match any digit.
anyDigit :: Parser Char
anyDigit = Parser \{ str: str, pos: i } fc sc -> case charAt i str of
  Just chr ->
    let chrS = fromChar chr
    in if Rx.test rxDigit chrS
       then sc chr { str: str, pos: i + 1 }
       else fc i (ParseError "Expected digit")
  Nothing -> fc i (ParseError "Unexpected EOF")
  where
  rxDigit :: Rx.Regex
  rxDigit = Rx.regex "^[0-9]" Rx.noFlags

noneOf :: Array Char -> Parser Char
noneOf ss = do
  c <- anyChar
  if c `notElem` ss
    then pure c
    else fail $ "Expected none of " <> show ss

-- | Match the specified string.
string :: String -> Parser String
string nt = Parser (\s fc sc -> case s of
  { str = str, pos = i } | indexOf' nt i str == Just i -> sc nt { str: str, pos: i + length nt }
  { pos = i } -> fc i (ParseError $ "Expected '" ++ nt ++ "'."))
