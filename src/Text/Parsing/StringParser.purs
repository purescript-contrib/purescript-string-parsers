module Text.Parsing.StringParser where

import Data.Maybe (Maybe(..))
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
-- Some elementary parsers
--

fail :: forall a. String -> Parser a
fail msg = Parser (\_ fc _ -> fc (ParseError msg))

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
  { str = str, pos = i } | indexOf' nt i str == i -> sc nt { str: str, pos: i + length nt }
  { pos = i } -> fc (ParseError $ "Expected '" ++ nt ++ "' at position " ++ show i ++ "."))

--
-- Parsing Combinators
--

many :: forall a. Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: forall a. Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a : as)

(<?>) :: forall a. Parser a -> String -> Parser a
(<?>) p msg = p <|> fail msg

fix :: forall a. (Parser a -> Parser a) -> Parser a
fix f = Parser (\s fc sc -> unParser (f (fix f)) s fc sc)

between :: forall a open close. Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  open
  a <- p
  close 
  return a

option :: forall a. a -> Parser a -> Parser a
option a p = p <|> return a

optional :: forall a. Parser a -> Parser {}
optional p = (p >>= \_ -> return {}) <|> return {}

optionMaybe :: forall a. Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

sepBy :: forall a sep. Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: forall a sep. Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  a <- p
  as <- many $ do
    sep
    p
  return (a : as)

sepEndBy :: forall a sep. Parser a -> Parser sep -> Parser [a]
sepEndBy p sep = sepEndBy1 p sep <|> return []

sepEndBy1 :: forall a sep. Parser a -> Parser sep -> Parser [a]
sepEndBy1 p sep = do
  a <- p
  (do sep
      as <- sepEndBy p sep
      return (a : as)) <|> return [a]

endBy1 :: forall a sep. Parser a -> Parser sep -> Parser [a]
endBy1 p sep = many1 $ do 
  a <- p
  sep
  return a

endBy :: forall a sep. Parser a -> Parser sep -> Parser [a]
endBy p sep = many $ do
  a <- p
  sep
  return a

chainr :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p f a = chainr1 p f <|> return a

chainl :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p f a = chainl1 p f <|> return a

chainl1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p f = do
  a <- p
  chainl1' p f a

chainl1' :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl1' p f a = (do f' <- f
                     a' <- p
                     chainl1' p f (f' a a')) <|> return a

chainr1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p f = do
  a <- p
  chainr1' p f a

chainr1' :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr1' p f a = (do f' <- f
                     a' <- chainr1 p f
                     return $ f' a a') <|> return a

choice :: forall a. [Parser a] -> Parser a
choice []   = fail "Nothing to parse"
choice [x]  = x
choice (x:xs) = x <|> choice xs
