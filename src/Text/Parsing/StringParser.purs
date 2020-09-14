-- | This module defines the `Parser` type of string parsers, and its instances.

module Text.Parsing.StringParser where

import Prelude

import Control.Apply (lift2)
import Control.MonadPlus (class MonadPlus, class MonadZero, class Alternative)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Plus (class Plus, class Alt)
import Control.Lazy (class Lazy)
import Data.Bifunctor (bimap, lmap)
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
newtype ParseError = ParseError String

instance showParseError :: Show ParseError where
  show (ParseError msg) = msg

derive instance eqParseError :: Eq ParseError

derive instance ordParseError :: Ord ParseError

-- | A parser is represented as a function which takes a pair of
-- | continuations for failure and success.
newtype Parser a = Parser (PosString -> Either { pos :: Pos, error :: ParseError } { result :: a, suffix :: PosString })

-- | Run a parser by providing success and failure continuations.
unParser :: forall a. Parser a -> PosString -> Either { pos :: Pos, error :: ParseError } { result :: a, suffix :: PosString }
unParser (Parser p) = p

-- | Run a parser for an input string, returning either an error or a result.
runParser :: forall a. Parser a -> String -> Either ParseError a
runParser (Parser p) s = bimap _.error _.result (p { str: s, pos: 0 })

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
fail msg = Parser \{ pos } -> Left { pos, error: ParseError msg }

-- | In case of error, the default behavior is to backtrack if no input was consumed.
-- |
-- | `try p` backtracks even if input was consumed.
try :: forall a. Parser a -> Parser a
try (Parser p) = Parser \(s@{ pos }) -> lmap (_ { pos = pos}) (p s)

instance semigroupParser :: Semigroup a => Semigroup (Parser a) where
  append = lift2 append

instance monoidParser :: Monoid a => Monoid (Parser a) where
  mempty = pure mempty
