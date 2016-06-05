-- | This module defines combinators for building string parsers.

module Text.Parsing.StringParser.Combinators where

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Rec.Class (tailRecM)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.List (reverse, List(..), singleton)
import Data.Maybe (Maybe(..))
import Text.Parsing.StringParser (Parser(..), fail, unParser)

-- | Read ahead without consuming input.
lookAhead :: forall a. Parser a -> Parser a
lookAhead (Parser p) = Parser \s ->
  case p s of
    Right { result } -> Right { result, suffix: s }
    left -> left

-- | Match zero or more times.
many :: forall a. Parser a -> Parser (List a)
many p = tailRecM go Nil
  where
  go :: List a -> Parser (Either (List a) (List a))
  go acc = do
    aa <- (Left <$> p) <|> pure (Right unit)
    pure $ bimap (flip Cons acc) (\_ -> reverse acc) aa

-- | Match one or more times.
many1 :: forall a. Parser a -> Parser (List a)
many1 p = Cons <$> p <*> many p


-- | Provide an error message in case of failure.
withError :: forall a. Parser a -> String -> Parser a
withError p msg = p <|> fail msg

infixl 3 withError as <?>

-- | Take the fixed point of a parser function. This function is sometimes useful when building recursive parsers.
fix :: forall a. (Parser a -> Parser a) -> Parser a
fix f = Parser \s -> unParser (f (fix f)) s

-- | Parse a string between opening and closing markers.
between :: forall a open close. Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  open
  a <- p
  close
  pure a

-- | Parse a value with a default value in case of failure.
option :: forall a. a -> Parser a -> Parser a
option a p = p <|> pure a

-- | Attempt to parse a value.
optional :: forall a. Parser a -> Parser Unit
optional p = (p >>= \_ -> pure unit) <|> pure unit

-- | Attempt to parse a value, pureing `Nothing` in case of failure.
optionMaybe :: forall a. Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

-- | Parse zero or more separated values.
sepBy :: forall a sep. Parser a -> Parser sep -> Parser (List a)
sepBy p sep = sepBy1 p sep <|> pure Nil

-- | Parse one or more separated values.
sepBy1 :: forall a sep. Parser a -> Parser sep -> Parser (List a)
sepBy1 p sep = do
  a <- p
  as <- many $ do
    sep
    p
  pure (Cons a as)

-- | Parse zero or more separated values, optionally ending with a separator.
sepEndBy :: forall a sep. Parser a -> Parser sep -> Parser (List a)
sepEndBy p sep = sepEndBy1 p sep <|> pure Nil

-- | Parse one or more separated values, optionally ending with a separator.
sepEndBy1 :: forall a sep. Parser a -> Parser sep -> Parser (List a)
sepEndBy1 p sep = do
  a <- p
  (do sep
      as <- sepEndBy p sep
      pure (Cons a as)) <|> pure (singleton a)

-- | Parse zero or more separated values, ending with a separator.
endBy1 :: forall a sep. Parser a -> Parser sep -> Parser (List a)
endBy1 p sep = many1 $ do
  a <- p
  sep
  pure a

-- | Parse one or more separated values, ending with a separator.
endBy :: forall a sep. Parser a -> Parser sep -> Parser (List a)
endBy p sep = many $ do
  a <- p
  sep
  pure a

-- | Parse zero or more values separated by a right-associative operator.
chainr :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p f a = chainr1 p f <|> pure a

-- | Parse zero or more values separated by a left-associative operator.
chainl :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p f a = chainl1 p f <|> pure a

-- | Parse one or more values separated by a left-associative operator.
chainl1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p f = do
  a <- p
  chainl1' p f a

-- | Parse one or more values separated by a left-associative operator.
chainl1' :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl1' p f a = (do f' <- f
                     a' <- p
                     chainl1' p f (f' a a')) <|> pure a

-- | Parse one or more values separated by a right-associative operator.
chainr1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p f = do
  a <- p
  chainr1' p f a

-- | Parse one or more values separated by a right-associative operator.
chainr1' :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr1' p f a = (do f' <- f
                     a' <- chainr1 p f
                     pure $ f' a a') <|> pure a

-- | Parse using any of a collection of parsers.
choice :: forall f a. (Foldable f) => f (Parser a) -> Parser a
choice = foldl (<|>) (fail "Nothing to parse")

-- | Parse values until a terminator.
manyTill :: forall a end. Parser a -> Parser end -> Parser (List a)
manyTill p end = scan
  where
  scan = (end *> pure Nil) <|> do x <- p
                                  xs <- scan
                                  pure (Cons x xs)
