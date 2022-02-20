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
import Data.String.CodePoints as SCP
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Text.Parsing.StringParser (Parser(..), try, fail)
import Text.Parsing.StringParser.Combinators (many, (<?>))
import Text.Parsing.StringParser.CodeUnits as CodeUnitsParser

-- | Match the end of the file.
eof :: Parser Unit
eof = Parser \s ->
  case s of
    { substr, posFromStart } | 0 < SCU.length substr -> Left { pos: posFromStart, error: "Expected EOF" }
    _ -> Right { result: unit, suffix: s }

-- | Match any character.
anyChar :: Parser Char
anyChar = Parser \{ substr, posFromStart } ->
  case SCP.codePointAt 0 substr of
    Just cp -> case toChar cp of
      Just chr -> Right { result: chr, suffix: { substr: SCP.drop 1 substr, posFromStart: posFromStart + 1 } }
      Nothing -> Left { pos: posFromStart, error: "CodePoint " <> show cp <> " is not a character" }
    Nothing -> Left { pos: posFromStart, error: "Unexpected EOF" }
  where
  toChar = fromCharCode <<< fromEnum

-- | Match any digit.
anyDigit :: Parser Char
anyDigit = try do
  c <- CodeUnitsParser.anyChar
  if c >= '0' && c <= '9' then pure c
  else fail $ "Character " <> show c <> " is not a digit"

-- | Match the specified string.
string :: String -> Parser String
string pattern = Parser \{ substr, posFromStart } ->
  let
    length = SCU.length pattern
    { before, after } = SCU.splitAt length substr
  in
    if before == pattern then Right { result: pattern, suffix: { substr: after, posFromStart: posFromStart + length } }
    else Left { pos: posFromStart, error: "Expected '" <> pattern <> "'." }

-- | Match a character satisfying the given predicate.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = try do
  c <- anyChar
  if f c then pure c
  else fail $ "Character " <> show c <> " did not satisfy predicate"

-- | Match the specified character.
char :: Char -> Parser Char
char c = satisfy (_ == c) <?> "Could not match character " <> show c

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
      fail $ "Text.Parsing.StringParser.String.regex': illegal regex " <> pat
    Right r ->
      matchRegex r
  where
  -- ensure the pattern only matches the current position in the parse
  pattern = "^(" <> pat <> ")"

  matchRegex :: Regex.Regex -> Parser String
  matchRegex r = Parser \{ substr, posFromStart } -> do
    case NEA.head <$> Regex.match r substr of
      Just (Just matched) ->
        Right { result: matched, suffix: { substr: SCU.drop (SCU.length matched) substr, posFromStart: posFromStart + SCU.length matched } }
      _ ->
        Left { pos: posFromStart, error: "no match" }
