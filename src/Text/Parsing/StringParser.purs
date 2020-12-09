-- | This module defines the `Parser` type of string parsers, and its instances.

module Text.Parsing.StringParser where

import Prelude

import Control.Apply (lift2)
import Control.MonadPlus (class MonadPlus, class MonadZero, class Alternative)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Plus (class Plus, class Alt)
import Control.Lazy (class Lazy)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))

-- | A position in an input string.
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
type ParseError = { error :: String, pos :: Pos }

-- | A parser is represented as a function that, when successful, returns
-- | a result and the position where the parse finished or, when it fails,
-- | a ParserError with more information on where and why it failed.
-- | See also `printParserError`.
newtype Parser a = Parser (PosString -> Either ParseError { result :: a, suffix :: PosString })

-- | Run a parser, allowing the caller to define where to start within the
-- | input `String` and what to do with the unchanged output of the Parser.
-- | See `runparser` for more typical usages.
unParser :: forall a. Parser a -> PosString -> Either ParseError { result :: a, suffix :: PosString }
unParser (Parser p) = p

-- | Run a parser for an input string. See also `printParserError`
-- | and `unParser` for more flexible usages.
runParser :: forall a. Parser a -> String -> Either ParseError a
runParser (Parser p) s = map _.result (p { str: s, pos: 0 })

-- | Prints a ParseError's the error message and the position of the error.
printParserError :: ParseError -> String
printParserError rec = rec.error <> "; pos = " <> show rec.pos

instance functorParser :: Functor Parser where
  map f (Parser p) = Parser (map (\{ result, suffix } -> { result: f result, suffix }) <<< p)

instance applyParser :: Apply Parser where
  apply (Parser p1) (Parser p2) = Parser \s -> do
    { result: f, suffix: s1 } <- p1 s
    { result: x, suffix: s2 } <- p2 s1
    pure { result: f x, suffix: s2 }

instance applicativeParser :: Applicative Parser where
  pure a = Parser \s -> Right { result: a, suffix: s }

instance altParser :: Alt Parser where
  alt (Parser p1) (Parser p2) = Parser \s ->
    case p1 s of
      Left { error, pos } | s.pos == pos -> p2 s
                          | otherwise -> Left { error, pos }
      right -> right

instance plusParser :: Plus Parser where
  empty = fail "No alternative"

instance alternativeParser :: Alternative Parser

instance bindParser :: Bind Parser where
  bind (Parser p) f = Parser \s -> do
    { result, suffix } <- p s
    unParser (f result) suffix

instance monadParser :: Monad Parser

instance monadZeroParser :: MonadZero Parser

instance monadPlusParser :: MonadPlus Parser

instance monadRecParser :: MonadRec Parser where
  tailRecM f a = Parser \str -> tailRecM (\st -> map split (unParser (f st.state) st.str)) { state: a, str }
    where
      split { result: Loop state, suffix: str } = Loop { state, str }
      split { result: Done b, suffix } = Done { result: b, suffix }

instance lazyParser :: Lazy (Parser a) where
  defer f = Parser $ \str -> unParser (f unit) str

-- | Fail with the specified message.
fail :: forall a. String -> Parser a
fail error = Parser \{ pos } -> Left { pos, error }

-- | In case of error, the default behavior is to backtrack if no input was consumed.
-- |
-- | `try p` backtracks even if input was consumed.
try :: forall a. Parser a -> Parser a
try (Parser p) = Parser \(s@{ pos }) -> lmap (_ { pos = pos}) (p s)

instance semigroupParser :: Semigroup a => Semigroup (Parser a) where
  append = lift2 append

instance monoidParser :: Monoid a => Monoid (Parser a) where
  mempty = pure mempty
