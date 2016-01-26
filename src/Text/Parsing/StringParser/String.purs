-- | Primitive parsers for strings.

module Text.Parsing.StringParser.String
  ( eof
  , anyChar
  , anyDigit
  , string
  , satisfy
  , char
  , whiteSpace
  , skipSpaces
  , oneOf
  , noneOf
  , lowerCaseChar
  , upperCaseChar
  , anyLetter
  , alphaNum
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (charAt, fromChar, length, indexOf', fromCharArray)
import Data.Char (toString, toCharCode)
import Data.Foldable (Foldable, foldMap, elem, notElem)
import Data.Array ((..))
import Control.Alt ((<|>))

import Text.Parsing.StringParser.Combinators (many, (<?>))
import Text.Parsing.StringParser

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
anyDigit = try do
  c <- anyChar
  if c >= '0' && c <= '9'
     then return c
     else fail $ "Character " <> toString c <> " is not a digit"

-- | Match the specified string.
string :: String -> Parser String
string nt = Parser (\s fc sc -> case s of
  { str = str, pos = i } | indexOf' nt i str == Just i -> sc nt { str: str, pos: i + length nt }
  { pos = i } -> fc i (ParseError $ "Expected '" ++ nt ++ "'."))

-- | Match a character satisfying the given predicate.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = try do
  c <- anyChar
  if f c
     then return c
     else fail $ "Character " <> toString c <> " did not satisfy predicate"

-- | Match the specified character.
char :: Char -> Parser Char
char c = satisfy (== c) <?> "Could not match character " <> toString c

-- | Match many whitespace characters.
whiteSpace :: Parser String
whiteSpace = do
  cs <- many (satisfy \ c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')
  return (foldMap toString cs)

-- | Skip many whitespace characters.
skipSpaces :: Parser Unit
skipSpaces = void whiteSpace

-- | Match one of the characters in the foldable structure.
oneOf :: forall f. (Foldable f) => f Char -> Parser Char
oneOf = satisfy <<< flip elem

-- | Match any character not in the foldable structure.
noneOf :: forall f. (Foldable f) => f Char -> Parser Char
noneOf = satisfy <<< flip notElem

-- | Match any lower case character.
lowerCaseChar :: Parser Char
lowerCaseChar = do
  c <- anyChar
  if toCharCode c `elem` (97 .. 122)
     then return c
     else fail $ "Expected a lower case character but found '" <> toString c <> "'"

-- | Match any upper case character.
upperCaseChar :: Parser Char
upperCaseChar = do
  c <- anyChar
  if toCharCode c `elem` (65 .. 90)
     then return c
     else fail $ "Expected an upper case character but found '" <> toString c <> "'"

-- | Match any letter.
anyLetter :: Parser Char
anyLetter = lowerCaseChar <|> upperCaseChar <?> "Expected a letter"

-- | Match a letter or a number.
alphaNum :: Parser Char
alphaNum = anyLetter <|> anyDigit <?> "Expected a letter or a number"
