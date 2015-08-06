## Module Parser

#### `satisfy`

``` purescript
satisfy :: (Char -> Boolean) -> Parser Char
```

#### `oneOf`

``` purescript
oneOf :: Array Char -> Parser Char
```

#### `char`

``` purescript
char :: Char -> Parser Char
```

#### `spaces`

``` purescript
spaces :: Parser _
```

#### `letter`

``` purescript
letter :: Parser Char
```

#### `number`

``` purescript
number :: Parser String
```

#### `symbol`

``` purescript
symbol :: Parser Char
```

#### `date`

``` purescript
date :: Parser String
```

#### `time`

``` purescript
time :: Parser String
```

#### `name`

``` purescript
name :: Parser String
```

#### `message`

``` purescript
message :: Parser String
```

#### `testParser`

``` purescript
testParser :: forall a. (Show a) => String -> Parser a -> String
```

#### `parseLine`

``` purescript
parseLine :: Parser IRCMessage
```

#### `readLog`

``` purescript
readLog :: forall a. Parser a -> String -> Maybe a
```

#### `parsableWith`

``` purescript
parsableWith :: (IRCMessage -> Boolean) -> String -> Boolean
```

#### `parsable`

``` purescript
parsable :: String -> Boolean
```


