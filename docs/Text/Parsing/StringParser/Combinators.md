## Module Text.Parsing.StringParser.Combinators

This module defines combinators for building string parsers.

#### `lookAhead`

``` purescript
lookAhead :: forall a. Parser a -> Parser a
```

Read ahead without consuming input.

#### `many`

``` purescript
many :: forall a. Parser a -> Parser (List a)
```

Match zero or more times.

#### `many1`

``` purescript
many1 :: forall a. Parser a -> Parser (List a)
```

Match one or more times.

#### `(<?>)`

``` purescript
(<?>) :: forall a. Parser a -> String -> Parser a
```

_left-associative / precedence -1_

Provide an error message in case of failure.

#### `fix`

``` purescript
fix :: forall a. (Parser a -> Parser a) -> Parser a
```

Take the fixed point of a parser function. This function is sometimes useful when building recursive parsers.

#### `between`

``` purescript
between :: forall a open close. Parser open -> Parser close -> Parser a -> Parser a
```

Parse a string between opening and closing markers.

#### `option`

``` purescript
option :: forall a. a -> Parser a -> Parser a
```

Parse a value with a default value in case of failure.

#### `optional`

``` purescript
optional :: forall a. Parser a -> Parser Unit
```

Attempt to parse a value.

#### `optionMaybe`

``` purescript
optionMaybe :: forall a. Parser a -> Parser (Maybe a)
```

Attempt to parse a value, returning `Nothing` in case of failure.

#### `sepBy`

``` purescript
sepBy :: forall a sep. Parser a -> Parser sep -> Parser (List a)
```

Parse zero or more separated values.

#### `sepBy1`

``` purescript
sepBy1 :: forall a sep. Parser a -> Parser sep -> Parser (List a)
```

Parse one or more separated values.

#### `sepEndBy`

``` purescript
sepEndBy :: forall a sep. Parser a -> Parser sep -> Parser (List a)
```

Parse zero or more separated values, optionally ending with a separator.

#### `sepEndBy1`

``` purescript
sepEndBy1 :: forall a sep. Parser a -> Parser sep -> Parser (List a)
```

Parse one or more separated values, optionally ending with a separator.

#### `endBy1`

``` purescript
endBy1 :: forall a sep. Parser a -> Parser sep -> Parser (List a)
```

Parse zero or more separated values, ending with a separator.

#### `endBy`

``` purescript
endBy :: forall a sep. Parser a -> Parser sep -> Parser (List a)
```

Parse one or more separated values, ending with a separator.

#### `chainr`

``` purescript
chainr :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
```

Parse zero or more values separated by a right-associative operator.

#### `chainl`

``` purescript
chainl :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
```

Parse zero or more values separated by a left-associative operator.

#### `chainl1`

``` purescript
chainl1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
```

Parse one or more values separated by a left-associative operator.

#### `chainl1'`

``` purescript
chainl1' :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
```

Parse one or more values separated by a left-associative operator.

#### `chainr1`

``` purescript
chainr1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
```

Parse one or more values separated by a right-associative operator.

#### `chainr1'`

``` purescript
chainr1' :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
```

Parse one or more values separated by a right-associative operator.

#### `choice`

``` purescript
choice :: forall f a. (Foldable f) => f (Parser a) -> Parser a
```

Parse using any of a collection of parsers.

#### `manyTill`

``` purescript
manyTill :: forall a end. Parser a -> Parser end -> Parser (List a)
```

Parse values until a terminator.


