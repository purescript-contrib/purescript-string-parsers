module Text.Parsing.StringParser where

import Data.String (charAt, length, take)
import Data.Either (Either(..))

-- 
-- Strings are represented as a string with an index from the
-- start of the string. 
--
-- { str: s, pos: n } is interpreted as the substring of s
-- starting at index n.
--
-- This allows us to avoid repeatedly finding substrings
-- every time we match a character.
--
type PosString = { str :: String, pos :: Number }

--
-- The type of parsing errors
--
data ParseError = ParseError String

instance showParseError :: Show ParseError where
  show (ParseError msg) = msg

--
-- A parser is represented as a function which takes a pair of 
-- continuations for failure and success.
--
data Parser a = Parser (forall r. PosString -> (ParseError -> r) -> (a -> PosString -> r) -> r)

unParser :: forall a r. Parser a -> PosString -> (ParseError -> r) -> (a -> PosString -> r) -> r
unParser (Parser p) = p

runParser :: forall a. Parser a -> String -> Either ParseError a
runParser p s = unParser p { str: s, pos: 0 } Left (\a _ -> Right a)

--
-- Parser type class instances
--

instance functorParser :: Functor Parser where
  (<$>) f p = Parser (\s fc sc -> 
    unParser p s fc (\a s' -> sc (f a) s'))

instance applyParser :: Apply Parser where
  (<*>) f x = Parser (\s fc sc -> 
    unParser f s fc (\f' s' ->
      unParser x s' fc (\x' s'' -> sc (f' x') s'')))

instance applicativeParser :: Applicative Parser where
  pure a = Parser (\s _ sc -> sc a s)

instance bindParser :: Bind Parser where
  (>>=) p f = Parser (\s fc sc ->
    unParser p s fc (\a s' ->
      unParser (f a) s' fc sc))

instance monadParser :: Monad Parser

instance alternativeParser :: Alternative Parser where
  empty = Parser (\_ fc _ -> fc (ParseError "No alternative"))
  (<|>) p1 p2 = Parser (\s fc sc -> 
    unParser p1 s (\_ ->
      unParser p2 s fc sc) sc)

--
-- Error handling combinator
--
(<?>) :: forall a. Parser a -> String -> Parser a
(<?>) p msg = Parser (\s fc sc -> unParser p s (\_ -> fc (ParseError msg)) sc)

--
-- Some elementary parsers
--
eof :: Parser {}
eof = Parser (\s fc sc -> case s of
  { str = str, pos = i } | i < length str -> fc (ParseError "Expected EOF") 
  _ -> sc {} s)

anyChar :: Parser String
anyChar = Parser (\s fc sc -> case s of
  { str = str, pos = i } | i < length str -> sc (charAt i str) { str: str, pos: i + 1 }
  _ -> fc (ParseError "Unexpected EOF"))

foreign import indexOf'
  "function indexOf$prime(x) {\
  \  return function(startAt) {\
  \    return function(s) {\
  \      return s.indexOf(x, startAt);\
  \    }; \
  \  }; \
  \}" :: String -> Number -> String -> Number

string :: String -> Parser String
string nt = Parser (\s fc sc -> case s of
  { str = str, pos = i } | indexOf' nt i str == 0 -> sc nt { str: str, pos: i + length nt }
  _ -> fc (ParseError $ "Expected '" ++ nt ++ "'"))

