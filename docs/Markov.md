## Module Markov

#### `empty`

``` purescript
empty :: forall a. MarkovChain a
```

This module is essentially a proof of existence for certain Markov Chains.
An algorithm is provided which takes a list of strings and constructs by induction a Markov Chain of the
appropriate type.

Afterwards, it is shown how to extract a message from such a chain.
We begin by defining the empty Markov Chain, since the construction below happens by induction on an input list
of strings.

#### `states`

``` purescript
states :: forall a. MarkovChain a -> States a
```

We need a way to extract the set of states from a given chain,

#### `transitions`

``` purescript
transitions :: forall a. MarkovChain a -> Transitions a
```

and the map of transitions.

#### `start`

``` purescript
start :: forall a. (Ord a) => MarkovChain a -> State a
```

We also need a way to get the distinguished starting state from a chain. Normally this algorithm would not
be safe, as it will result in a runtime error if the set of states has no distinguished state. But using
the method below, this state will always exist by construction.

#### `possibleTransitions`

``` purescript
possibleTransitions :: forall a. (Ord a) => MarkovChain a -> State a -> List (State a)
```

Since we store transitions as a map of states to lists of states, getting the image of for a given state is merely
looking up the value for a key, when the key is the given state.

#### `addState`

``` purescript
addState :: forall a. (Ord a) => a -> MarkovChain a -> MarkovChain a
```

To build up the chain, we need a method of adding a new state to the chain's set of states. If the set is empty,
we insert the element as a distinguished state. Otherwise it is normal.

#### `getStateCtor`

``` purescript
getStateCtor :: forall a. (Ord a) => a -> States a -> Maybe (a -> State a)
```

Adding transitions is more complicated. Since the last k-gram constructed from the input list will have the
distinguished state as a transition, there needs to be a way to get the State constructor of an element in a set.
In other words, we can't assume the destination k-gram should be added as a nondistinguished state.

#### `unsafeGetStateCtor`

``` purescript
unsafeGetStateCtor :: forall a. (Ord a) => a -> States a -> a -> State a
```

Because our construction will first add k-grams to the set of states, before adding them to the map of transitions,
we know when we can get the constructor without needing to wrap it in a Maybe type.

#### `isSrc`

``` purescript
isSrc :: forall a. (Ord a) => a -> Transitions a -> Boolean
```

Finally, we need a way to determine if a given element is the source of a transition.

#### `addTransition`

``` purescript
addTransition :: forall a. (Ord a) => a -> a -> MarkovChain a -> MarkovChain a
```

Now we show how to add a transition `(src, dest)` to a chain. If `src` is already the source of a transition, we
will modify the list of its destinations to include `dest`.
Otherwise, we add in a fresh key-value pair to the map.

#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> a -> MarkovChain a -> MarkovChain a
```

We can combine the acts of adding a state and a transition given two elements.

#### `nextState`

``` purescript
nextState :: forall a e. (Ord a) => MarkovChain a -> State a -> Eff (random :: RANDOM | e) (State a)
```

We also need a method of choosing which destination to jump to given a state as a source of a transition.

#### `mkMarkovChain`

``` purescript
mkMarkovChain :: forall a. (Ord a) => Int -> List a -> MarkovChain (List a)
```

The induction itself.
Base case: We start with an empty markov chain. If the input list is empty, we return the empty chain.
If it is a singleton, we only add in a reflexive transition.
Inductive case: The input list has `k >= 2` elements. We add the first element as a state, and the first two
elements as the source/destination of a transition, respectively. Then we continue with the second element and
the tail of the input list.

#### `createPath`

``` purescript
createPath :: forall a e. (Ord a) => Int -> MarkovChain a -> Eff (random :: RANDOM | e) (List (State a))
```

Now we can create a proper chain (well-ordering) by starting at the distinguished state and choosing uniformly
at random the next state from the list of possible transition destinations.

#### `showPath`

``` purescript
showPath :: forall a. (Show a) => States (List a) -> String
```

#### `showPathOfStrings`

``` purescript
showPathOfStrings :: List (State (List String)) -> String
```

#### `createStringFromChain`

``` purescript
createStringFromChain :: forall e. Int -> Int -> List String -> Eff (random :: RANDOM | e) String
```


