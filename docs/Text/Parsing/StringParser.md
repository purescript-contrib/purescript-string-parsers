## Module Text.Parsing.StringParser

This module defines the `Parser` type of string parsers, and its instances.

#### `Pos`

``` purescript
type Pos = Int
```

A poition in an input string.

#### `PosString`

``` purescript
type PosString = { str :: String, pos :: Pos }
```

Strings are represented as a string with an index from the
start of the string.

`{ str: s, pos: n }` is interpreted as the substring of `s`
starting at index n.

This allows us to avoid repeatedly finding substrings
every time we match a character.

#### `ParseError`

``` purescript
data ParseError
  = ParseError String
```

The type of parsing errors.

##### Instances
``` purescript
instance showParseError :: Show ParseError
instance eqParseError :: Eq ParseError
```

#### `Parser`

``` purescript
data Parser a
  = Parser (forall r. PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r)
```

A parser is represented as a function which takes a pair of
continuations for failure and success.

##### Instances
``` purescript
instance functorParser :: Functor Parser
instance applyParser :: Apply Parser
instance applicativeParser :: Applicative Parser
instance altParser :: Alt Parser
instance plusParser :: Plus Parser
instance alternativeParser :: Alternative Parser
instance bindParser :: Bind Parser
instance monadParser :: Monad Parser
instance monadPlusParser :: MonadPlus Parser
```

#### `unParser`

``` purescript
unParser :: forall a r. Parser a -> PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r
```

Run a parser by providing success and failure continuations.

#### `runParser`

``` purescript
runParser :: forall a. Parser a -> String -> Either ParseError a
```

Run a parser for an input string, returning either an error or a result.

#### `fail`

``` purescript
fail :: forall a. String -> Parser a
```

Fail with the specified message.

#### `try`

``` purescript
try :: forall a. Parser a -> Parser a
```

In case of error, the default behavior is to backtrack if no input was consumed.

`try p` backtracks even if input was consumed.


