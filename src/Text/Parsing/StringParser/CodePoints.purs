-- | Primitive parsers for strings, parsing based on code points.
-- |
-- | These functions will be much slower than the `CodeUnits` alternatives, but
-- | will behave correctly in the presence of Unicode characters made up of
-- | multiple code units.
module Text.Parsing.StringParser.CodePoints
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
  , regex
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Array.NonEmpty as NEA
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable, foldMap, elem, notElem)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (codePointAt, drop, indexOf', length, stripPrefix)
import Data.String.CodeUnits (singleton)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Text.Parsing.StringParser (Parser(..), try, fail)
import Text.Parsing.StringParser.Combinators (many, (<?>))

-- | Match the end of the file.
eof :: Parser Unit
eof = Parser \s ->
  case s of
    { str, pos } | pos < length str -> Left { pos, error: "Expected EOF" }
    _ -> Right { result: unit, suffix: s }

-- | Match any character.
anyChar :: Parser Char
anyChar = Parser \{ str, pos } ->
  case codePointAt pos str of
    Just cp -> case toChar cp of
      Just chr -> Right { result: chr, suffix: { str, pos: pos + 1 } }
      Nothing -> Left { pos, error: "CodePoint " <> show cp <> " is not a character" }
    Nothing -> Left { pos, error: "Unexpected EOF" }
  where
    toChar = fromCharCode <<< fromEnum

-- | Match any digit.
anyDigit :: Parser Char
anyDigit = try do
  c <- anyChar
  if c >= '0' && c <= '9'
     then pure c
     else fail $ "Character " <> show c <> " is not a digit"

-- | Match the specified string.
string :: String -> Parser String
string nt = Parser \s ->
  case s of
    { str, pos } | indexOf' (Pattern nt) pos str == Just pos -> Right { result: nt, suffix: { str, pos: pos + length nt } }
    { pos } -> Left { pos, error: "Expected '" <> nt <> "'." }

-- | Match a character satisfying the given predicate.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = try do
  c <- anyChar
  if f c
     then pure c
     else fail $ "Character " <> show c <> " did not satisfy predicate"

-- | Match the specified character.
char :: Char -> Parser Char
char c = satisfy (_ == c) <?> "Could not match character " <> show c

-- | Match many whitespace characters.
whiteSpace :: Parser String
whiteSpace = do
  cs <- many (satisfy \ c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')
  pure (foldMap singleton cs)

-- | Skip many whitespace characters.
skipSpaces :: Parser Unit
skipSpaces = void whiteSpace

-- | Match one of the characters in the foldable structure.
oneOf :: forall f. Foldable f => f Char -> Parser Char
oneOf = satisfy <<< flip elem

-- | Match any character not in the foldable structure.
noneOf :: forall f. Foldable f => f Char -> Parser Char
noneOf = satisfy <<< flip notElem

-- | Match any lower case character.
lowerCaseChar :: Parser Char
lowerCaseChar = try do
  c <- anyChar
  if toCharCode c `elem` (97 .. 122)
     then pure c
     else fail $ "Expected a lower case character but found " <> show c

-- | Match any upper case character.
upperCaseChar :: Parser Char
upperCaseChar = try do
  c <- anyChar
  if toCharCode c `elem` (65 .. 90)
     then pure c
     else fail $ "Expected an upper case character but found " <> show c

-- | Match any letter.
anyLetter :: Parser Char
anyLetter = lowerCaseChar <|> upperCaseChar <?> "Expected a letter"

-- | Match a letter or a number.
alphaNum :: Parser Char
alphaNum = anyLetter <|> anyDigit <?> "Expected a letter or a number"

-- | match the regular expression
regex :: String -> Parser String
regex pat =
  case Regex.regex pattern noFlags of
    Left _ ->
      fail $ "Text.Parsing.StringParser.String.regex': illegal regex " <> pat
    Right r ->
      matchRegex r
  where
    -- ensure the pattern only matches the current position in the parse
    pattern =
      case stripPrefix (Pattern "^") pat of
        Nothing ->
          "^" <> pat
        _ ->
          pat
    matchRegex :: Regex.Regex -> Parser String
    matchRegex r =
      Parser \{ str, pos } ->
        let
          remainder = drop pos str
        in
          case NEA.head <$> Regex.match r remainder of
            Just (Just matched)  ->
              Right { result: matched, suffix: { str, pos: pos + length matched } }
            _ ->
              Left { pos, error: "no match" }
