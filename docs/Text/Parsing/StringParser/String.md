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

#### `satisfy`

``` purescript
satisfy :: (Char -> Boolean) -> Parser Char
```

Match a character satisfying the given predicate.

#### `char`

``` purescript
char :: Char -> Parser Char
```

Match the specified character.

#### `whiteSpace`

``` purescript
whiteSpace :: Parser String
```

Match many whitespace characters.

#### `skipSpaces`

``` purescript
skipSpaces :: Parser Unit
```

Skip many whitespace characters.

#### `oneOf`

``` purescript
oneOf :: forall f. (Foldable f) => f Char -> Parser Char
```

Match one of the characters in the foldable structure.

#### `noneOf`

``` purescript
noneOf :: forall f. (Foldable f) => f Char -> Parser Char
```

Match any character not in the foldable structure.

#### `lowerCaseChar`

``` purescript
lowerCaseChar :: Parser Char
```

Match any lower case character.

#### `upperCaseChar`

``` purescript
upperCaseChar :: Parser Char
```

Match any upper case character.

#### `anyLetter`

``` purescript
anyLetter :: Parser Char
```

Match any letter.

#### `alphaNum`

``` purescript
alphaNum :: Parser Char
```

Match a letter or a number.


