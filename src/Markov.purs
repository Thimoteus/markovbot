module Markov where

import Prelude
import Types
import Util

import Data.List hiding (insert)
import Data.Int
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Either
import qualified Data.String as S
import qualified Data.List.Unsafe as U
import qualified Data.Map as M
import qualified Data.Set as V

import Control.MonadPlus
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Random
import Control.Monad.Rec.Class
import Control.Monad.Eff.Console

-- | This module is essentially a proof of existence for certain Markov Chains.
-- | An algorithm is provided which takes a list of strings and constructs by induction a Markov Chain of the
-- | appropriate type.
-- |
-- | Afterwards, it is shown how to extract a message from such a chain.

-- | We begin by defining the empty Markov Chain, since the construction below happens by induction on an input list
-- | of strings.
empty :: forall a. MarkovChain a
empty = MarkovChain V.empty M.empty

-- | We need a way to extract the set of states from a given chain,
states :: forall a. MarkovChain a -> States a
states (MarkovChain s _) = s

-- | and the map of transitions.
transitions :: forall a. MarkovChain a -> Transitions a
transitions (MarkovChain _ t) = t

-- | We also need a way to get the distinguished starting state from a chain. Normally this algorithm would not
-- | be safe, as it will result in a runtime error if the set of states has no distinguished state. But using
-- | the method below, this state will always exist by construction.
start :: forall a. (Ord a) => MarkovChain a -> State a
start (MarkovChain ss _) = go (V.toList ss) where
  go (Cons x@(Start _) _) = x
  go (Cons _ rest) = go rest

-- | Since we store transitions as a map of states to lists of states, getting the image of for a given state is merely
-- | looking up the value for a key, when the key is the given state.
possibleTransitions :: forall a. (Ord a) => MarkovChain a -> State a -> List (State a)
possibleTransitions chain curr = maybe (return curr) id $ M.lookup curr $ transitions chain

-- | To build up the chain, we need a method of adding a new state to the chain's set of states. If the set is empty,
-- | we insert the element as a distinguished state. Otherwise it is normal.
addState :: forall a. (Ord a) => a -> MarkovChain a -> MarkovChain a
addState s (MarkovChain ss ts) | V.isEmpty ss = MarkovChain (V.singleton (Start s)) ts
                               | otherwise = MarkovChain (V.insert (State s) ss) ts

-- | Adding transitions is more complicated. Since the last k-gram constructed from the input list will have the
-- | distinguished state as a transition, there needs to be a way to get the State constructor of an element in a set.
-- | In other words, we can't assume the destination k-gram should be added as a nondistinguished state.
getStateCtor :: forall a. (Ord a) => a -> States a -> Maybe (a -> State a)
getStateCtor x xs
  | V.member (State x) xs = Just State
  | V.member (Start x) xs = Just Start
  | otherwise = Nothing

-- | Because our construction will first add k-grams to the set of states, before adding them to the map of transitions,
-- | we know when we can get the constructor without needing to wrap it in a Maybe type.
unsafeGetStateCtor :: forall a. (Ord a) => a -> States a -> a -> State a
unsafeGetStateCtor x xs
  | V.member (State x) xs = State
  | V.member (Start x) xs = Start

-- | Finally, we need a way to determine if a given element is the source of a transition.
isSrc :: forall a. (Ord a) => a -> Transitions a -> Boolean
isSrc x ts = M.member (State x) ts || M.member (Start x) ts

-- | Now we show how to add a transition `(src, dest)` to a chain. If `src` is already the source of a transition, we
-- | will modify the list of its destinations to include `dest`.
-- | Otherwise, we add in a fresh key-value pair to the map.
addTransition :: forall a. (Ord a) => a -> a -> MarkovChain a -> MarkovChain a
addTransition src dest chain@(MarkovChain ss ts)
  | isSrc src ts = case getStateCtor dest ss of
                        Just c -> MarkovChain ss $ M.update (\ lst -> Just (c dest : lst)) (unsafeGetStateCtor src ss src) ts
                        Nothing -> MarkovChain ss $ M.update (\ lst -> Just (State dest : lst)) (unsafeGetStateCtor src ss src) ts
  | otherwise = case getStateCtor dest ss of
                     Just c -> MarkovChain ss $ M.insert (unsafeGetStateCtor src ss src) (singleton $ c dest) ts
                     Nothing -> MarkovChain ss $ M.insert (unsafeGetStateCtor src ss src) (singleton $ State dest) ts

-- | We can combine the acts of adding a state and a transition given two elements.
insert :: forall a. (Ord a) => a -> a -> MarkovChain a -> MarkovChain a
insert src dest chain = chain # addState src # addTransition src dest

-- | We also need a method of choosing which destination to jump to given a state as a source of a transition.
nextState :: forall a e. (Ord a) => MarkovChain a -> State a -> Eff ( random :: RANDOM | e ) (State a)
nextState chain curr = do
  maybeState <- choose $ possibleTransitions chain curr
  maybe (return $ start chain) return maybeState

-- | The induction itself.
-- | Base case: We start with an empty markov chain. If the input list is empty, we return the empty chain.
-- | If it is a singleton, we only add in a reflexive transition.
-- | Inductive case: The input list has `k >= 2` elements. We add the first element as a state, and the first two
-- | elements as the source/destination of a transition, respectively. Then we continue with the second element and
-- | the tail of the input list.
mkMarkovChain :: forall a. (Ord a) => Int -> List a -> MarkovChain (List a)
mkMarkovChain k xs = build empty $ kgram k xs
  where
    build :: MarkovChain (List a) -> List (List a) -> MarkovChain (List a)
    build chain Nil = chain
    build chain (Cons x (Cons y rest)) = build (chain # insert x y) (y : rest)
    build chain (Cons x Nil)
      | V.isEmpty $ states chain = insert x x chain
      | otherwise = insert x (fromState $ start chain) chain

-- | Now we can create a proper chain (well-ordering) by starting at the distinguished state and choosing uniformly
-- | at random the next state from the list of possible transition destinations.
createPath :: forall a e. (Ord a) => Int -> MarkovChain a -> (a -> Boolean) -> Number -> Eff ( random :: RANDOM | e ) (List (State a))
createPath n chain term p = tailRecM createPath' { count: n, lst: singleton $ start chain }
  where
    createPath' { count: 0 , lst: acc } = return $ Right acc
    createPath' { count: k, lst: acc@(Cons top _) }
      | term $ fromState top = do
        continue <- random
        if continue < p
          then return $ Right acc
          else nextState chain top >>= \next -> return $ Left { count: k - 1, lst: next : acc }
      | otherwise = do
        next <- nextState chain top
        return $ Left { count: k - 1, lst: next : acc }

showPath :: forall a. (Show a) => States (List a) -> String
showPath = S.joinWith "" <<< fromList <<< reverse <<< map (\ (State x) -> show $ U.last x) <<< V.toList

showPathOfStrings :: List (State (List String)) -> String
showPathOfStrings = S.joinWith "" <<< fromList <<< extractStrings <<< reverse
  where
    extractStrings :: List (State (List String)) -> List String
    extractStrings (Cons x xs) = (S.joinWith "" $ fromList $ fromState x) : (map (U.last <<< fromState) xs)
