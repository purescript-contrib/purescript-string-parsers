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
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (charAt, fromChar, length, indexOf', fromCharArray)
import Data.Char (toString)
import Data.Foldable (Foldable, foldMap, elem, notElem)

import Text.Parsing.StringParser.Combinators (many, (<?>))
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
