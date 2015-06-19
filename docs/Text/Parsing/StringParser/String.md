## Module Text.Parsing.StringParser.String

Primitive parsers for strings.

#### `eof`

``` purescript
eof :: Parser Unit
```

Match the end of the file.

#### `anyChar`

``` purescript
anyChar :: Parser Char
```

Match any character.

#### `anyDigit`

``` purescript
anyDigit :: Parser Char
```

Match any digit.

#### `string`

``` purescript
string :: String -> Parser String
```

Match the specified string.


