## Module Util

#### `mu`

``` purescript
mu :: forall a. (a -> Boolean) -> List a -> Maybe a
```

find the first element in a list with a property. Useful if the list is increasing or decreasing.

#### `mu'`

``` purescript
mu' :: forall a. (a -> Boolean) -> List a -> Maybe a
```

#### `normalize`

``` purescript
normalize :: List Int -> List Number
```

given a list of integers, normalize it so the largest element is 1

#### `choose`

``` purescript
choose :: forall a e. List a -> Eff (random :: RANDOM | e) (Maybe a)
```

#### `unsafeChoose`

``` purescript
unsafeChoose :: forall a e. List a -> Eff (random :: RANDOM | e) a
```

#### `kgram`

``` purescript
kgram :: forall a. Int -> List a -> List (List a)
```

given a list of elements, split it up into a list of overlapping (if k > 1) sublists of length k

#### `cut`

``` purescript
cut :: forall a e. Array a -> Eff (random :: RANDOM | e) (Array a)
```

#### `runEpochMilliseconds`

``` purescript
runEpochMilliseconds :: Milliseconds -> Int
```

#### `alphabetize`

``` purescript
alphabetize :: String -> List String
```


