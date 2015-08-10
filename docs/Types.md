## Module Types

#### `IRCMessage`

``` purescript
type IRCMessage = { name :: String, message :: String }
```

#### `BotInfo`

``` purescript
type BotInfo = { host :: Host, nick :: Nick, name :: String, chan :: Channel, keywords :: Array String, password :: String }
```

#### `State`

``` purescript
data State a
  = State a
  | Start a
```

##### Instances
``` purescript
instance stateEq :: (Eq a) => Eq (State a)
instance stateShow :: (Show a) => Show (State a)
instance ordState :: (Ord a) => Ord (State a)
instance functorState :: Functor State
```

#### `States`

``` purescript
type States a = Set (State a)
```

#### `Transitions`

``` purescript
type Transitions a = Map (State a) (List (State a))
```

#### `MarkovChain`

``` purescript
data MarkovChain a
  = MarkovChain (States a) (Transitions a)
```

##### Instances
``` purescript
instance markovchainEq :: (Eq a) => Eq (MarkovChain a)
instance markovChainShow :: (Show a) => Show (MarkovChain a)
instance semigroupMarkovChain :: (Ord a) => Semigroup (MarkovChain a)
instance monoidMarkovChain :: (Ord a) => Monoid (MarkovChain a)
```

#### `showList`

``` purescript
showList :: forall a. (Show a) => List a -> String
```

#### `showListString`

``` purescript
showListString :: List String -> String
```

#### `fromState`

``` purescript
fromState :: forall a. State a -> a
```

#### `showMap`

``` purescript
showMap :: forall k v. (Show k, Show v) => Map k (List v) -> String
```

#### `showSet`

``` purescript
showSet :: forall e. (Show e) => Set e -> String
```

#### `mapStates`

``` purescript
mapStates :: forall a b. (Ord a, Ord b) => (a -> b) -> States a -> States b
```

#### `mapTransition`

``` purescript
mapTransition :: forall a b. (Ord a, Ord b) => (a -> b) -> Map a (List a) -> Map b (List b)
```

#### `mapMarkovChain`

``` purescript
mapMarkovChain :: forall a b. (Ord a, Ord b) => (a -> b) -> MarkovChain a -> MarkovChain b
```


