## Module IRC

#### `getMessage`

``` purescript
getMessage :: forall e. Int -> Array (MarkovChain (List String)) -> Eff (random :: RANDOM | e) String
```

#### `containsKeywords`

``` purescript
containsKeywords :: String -> Array String -> Boolean
```

#### `lmrHandler`

``` purescript
lmrHandler :: forall e. BotInfo -> Array (MarkovChain (List String)) -> Aff (irc :: IRC, console :: CONSOLE | e) Unit
```

#### `lmrSimHandler`

``` purescript
lmrSimHandler :: forall e. BotInfo -> Array (MarkovChain (List String)) -> Aff (irc :: IRC, console :: CONSOLE | e) Unit
```

#### `runBot`

``` purescript
runBot :: BotInfo -> _
```


