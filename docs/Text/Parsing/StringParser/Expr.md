## Module Text.Parsing.StringParser.Expr

This module defines helper functions for defining parsers using operator tables.

#### `Assoc`

``` purescript
data Assoc
  = AssocNone
  | AssocLeft
  | AssocRight
```

Operator associativity types.

#### `Operator`

``` purescript
data Operator a
  = Infix (Parser (a -> a -> a)) Assoc
  | Prefix (Parser (a -> a))
  | Postfix (Parser (a -> a))
```

Operator types: infix, prefix, postfix.

#### `OperatorTable`

``` purescript
type OperatorTable a = Array (Array (Operator a))
```

An operator table arranges operators into precedence groups.

#### `buildExprParser`

``` purescript
buildExprParser :: forall a. OperatorTable a -> Parser a -> Parser a
```


