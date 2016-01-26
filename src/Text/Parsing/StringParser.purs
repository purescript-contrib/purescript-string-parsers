-- | This module defines the `Parser` type of string parsers, and its instances.

module Text.Parsing.StringParser where

import Prelude

import Data.Either (Either(..))

import Control.Alt
import Control.Alternative
import Control.MonadPlus
import Control.Plus

-- | A poition in an input string.
type Pos = Int

-- | Strings are represented as a string with an index from the
-- | start of the string.
-- | 
-- | `{ str: s, pos: n }` is interpreted as the substring of `s`
-- | starting at index n.
-- | 
-- | This allows us to avoid repeatedly finding substrings
-- | every time we match a character.
type PosString = { str :: String, pos :: Pos }

-- | The type of parsing errors.
data ParseError = ParseError String

instance showParseError :: Show ParseError where
  show (ParseError msg) = msg

instance eqParseError :: Eq ParseError where
  eq (ParseError x) (ParseError y) = x == y

-- | A parser is represented as a function which takes a pair of
-- | continuations for failure and success.
data Parser a = Parser (forall r. PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r)

-- | Run a parser by providing success and failure continuations.
unParser :: forall a r. Parser a -> PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r
unParser (Parser p) = p

-- | Run a parser for an input string, returning either an error or a result.
runParser :: forall a. Parser a -> String -> Either ParseError a
runParser p s = unParser p { str: s, pos: 0 } (\_ err -> Left err) (\a _ -> Right a)

instance functorParser :: Functor Parser where
  map f p = Parser (\s fc sc ->
    unParser p s fc (\a s' -> sc (f a) s'))

instance applyParser :: Apply Parser where
  apply f x = Parser (\s fc sc ->
    unParser f s fc (\f' s' ->
      unParser x s' fc (\x' s'' -> sc (f' x') s'')))

instance applicativeParser :: Applicative Parser where
  pure a = Parser (\s _ sc -> sc a s)

instance altParser :: Alt Parser where
  alt p1 p2 = Parser (\s fc sc -> 
    unParser p1 s (\_ _ -> unParser p2 s fc sc) sc)

instance plusParser :: Plus Parser where
  empty = fail "No alternative"

instance alternativeParser :: Alternative Parser

instance bindParser :: Bind Parser where
  bind p f = Parser (\s fc sc ->
    unParser p s fc (\a s' ->
      unParser (f a) s' fc sc))

instance monadParser :: Monad Parser

instance monadPlusParser :: MonadPlus Parser

-- | Fail with the specified message.
fail :: forall a. String -> Parser a
fail msg = Parser (\{ pos = pos } fc _ -> fc pos (ParseError msg))

-- | In case of error, the default behavior is to backtrack if no input was consumed.
-- |
-- | `try p` backtracks even if input was consumed.
try :: forall a. Parser a -> Parser a
try p = Parser (\(s@{ pos = pos }) fc sc -> unParser p s (\_ -> fc pos) sc)
