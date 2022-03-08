-- | Primitive parsers for strings, parsing based on code points.
-- |
-- | These functions will be much slower than the `CodeUnits` alternatives, but
-- | will behave correctly in the presence of Unicode characters made up of
-- | multiple code units.
module StringParser.CodePoints
  ( eof
  , anyChar
  , anyCodePoint
  , anyDigit
  , string
  , satisfy
  , satisfyCodePoint
  , char
  , codePoint
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
import Data.String (CodePoint)
import Data.String.CodePoints as SCP
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import StringParser.Parser (Parser(..), fail)
import StringParser.CodeUnits as CodeUnitsParser
import StringParser.Combinators (try, many, (<?>))

-- | Match the end of the file.
eof :: Parser Unit
eof = Parser \s ->
  case s of
    { substring, position } | 0 < SCP.length substring -> Left { pos: position, error: "Expected EOF" }
    _ -> Right { result: unit, suffix: s }

-- | Match any character from the Basic Multilingual Plane.
anyChar :: Parser Char
anyChar = do
  cc <- anyCodePoint <#> fromEnum
  case fromCharCode cc of
    Just chr ->
      -- the `fromCharCode` function doesn't check if this is beyond the
      -- BMP, so we check that ourselves.
      -- https://github.com/purescript/purescript-strings/issues/153
      if cc > 65535 -- BMP
      then notAChar cc
      else pure chr
    Nothing -> notAChar cc
  where
  notAChar cc = fail $ "Code point " <> show cc <> " is not a character"

-- | Match any code point.
anyCodePoint :: Parser CodePoint
anyCodePoint = Parser \{ substring, position } ->
  case SCP.uncons substring of
    Nothing -> Left { pos: position, error: "Unexpected EOF" }
    Just { head, tail } -> Right { result: head, suffix: { substring: tail, position: position + 1 } }

-- | Match any digit.
anyDigit :: Parser Char
anyDigit = try do
  c <- CodeUnitsParser.anyChar
  if c >= '0' && c <= '9' then pure c
  else fail $ "Character " <> show c <> " is not a digit"

-- | Match the specified string.
string :: String -> Parser String
string pattern = Parser \{ substring, position } ->
  let
    length = SCP.length pattern
    { before, after } = SCP.splitAt length substring
  in
    if before == pattern then Right { result: pattern, suffix: { substring: after, position: position + length } }
    else Left { pos: position, error: "Expected '" <> pattern <> "'." }

-- | Match a character satisfying the given predicate.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = try do
  c <- anyChar
  if f c then pure c
  else fail $ "Character " <> show c <> " did not satisfy predicate"

-- | Match a code point satisfying the given predicate.
satisfyCodePoint :: (CodePoint -> Boolean) -> Parser CodePoint
satisfyCodePoint f = try do
  cp <- anyCodePoint
  if f cp then pure cp
  else fail $ "Code point " <> show cp <> " did not satisfy predicate"

-- | Match the specified character.
char :: Char -> Parser Char
char c = satisfy (_ == c) <?> "Could not match character " <> show c

-- | Match the specified code point.
codePoint :: CodePoint -> Parser CodePoint
codePoint c = satisfyCodePoint (_ == c) <?> "Could not match code point " <> show c

-- | Match many whitespace characters.
whiteSpace :: Parser String
whiteSpace = do
  cs <- many (satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')
  pure (foldMap SCU.singleton cs)

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
  c <- CodeUnitsParser.anyChar
  if toCharCode c `elem` (97 .. 122) then pure c
  else fail $ "Expected a lower case character but found " <> show c

-- | Match any upper case character.
upperCaseChar :: Parser Char
upperCaseChar = try do
  c <- CodeUnitsParser.anyChar
  if toCharCode c `elem` (65 .. 90) then pure c
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
      fail $ "StringParser.String.regex': illegal regex " <> pat
    Right r ->
      matchRegex r
  where
  -- ensure the pattern only matches the current position in the parse
  pattern = "^(" <> pat <> ")"

  matchRegex :: Regex.Regex -> Parser String
  matchRegex r = Parser \{ substring, position } -> do
    case NEA.head <$> Regex.match r substring of
      Just (Just matched) ->
        Right { result: matched, suffix: { substring: SCP.drop (SCP.length matched) substring, position: position + SCP.length matched } }
      _ ->
        Left { pos: position, error: "no match" }
