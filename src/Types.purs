module Types where

import Prelude

import Data.List
import Data.Tuple
import Data.Either
import Data.Monoid
import qualified Data.Set as V
import qualified Data.Map as M

import Control.MonadPlus
import Control.Monad.Eff.Random
import Control.Monad.Rec.Class

type IRCMessage = { name :: String, message :: String }

newtype State a = State a
data Transition a = Transition (State a) (State a)
type States a = List (State a)
type Transitions a = List (Transition a)
data MarkovChain a = MarkovChain (States a) (Transitions a)

instance stateEq :: (Eq a) => Eq (State a) where
  eq (State x) (State y) = x == y
  eq _ _ = false

instance transitionEq :: (Eq a) => Eq (Transition a) where
  eq (Transition s d) (Transition s' d') = s == s' && d == d'

instance markovchainEq :: (Eq a) => Eq (MarkovChain a) where
  eq (MarkovChain ss ts) (MarkovChain ss' ts') = ss == ss' && ts == ts'

instance stateShow :: (Show a) => Show (State a) where
  show (State s) = "State " ++ show s

instance transitionShow :: (Show a) => Show (Transition a) where
  show (Transition s d) = show s ++ " -> " ++ show d

instance markovChainShow :: (Show a) => Show (MarkovChain a) where
  show (MarkovChain states transitions) = "MarkovChain {" ++ showList states ++ "}, {" ++ showList transitions ++ "}"

showList :: forall a. (Show a) => List a -> String
showList xs = "(" ++ tailRec go { lst: xs, accum: "" } ++ ")" where
  go { lst: Nil, accum: acc } = Right acc
  go { lst: Cons x xs, accum: "" } = Left { lst: xs, accum: show x }
  go { lst: Cons x xs, accum: acc } = Left { lst: xs, accum: acc ++ ", " ++ show x }

showListString :: List String -> String
showListString xs = "(" ++ tailRec go { lst: xs, accum: "" } ++ ")" where
  go { lst: Nil, accum: acc } = Right acc
  go { lst: Cons x xs, accum: "" } = Left { lst: xs, accum: x}
  go { lst: Cons x xs, accum: acc } = Left { lst: xs, accum: acc ++ ", " ++ x }

instance functorTransition :: Functor Transition where
  map f (Transition (State s) (State d)) = Transition (State (f s)) (State (f d))

instance functorState :: Functor State where
  map f (State s) = State (f s)

instance functorMarkovChain :: Functor MarkovChain where
  map f (MarkovChain states transs) = MarkovChain (map (map f) states) (map (map f) transs)

instance semigroupMarkovChain :: Semigroup (MarkovChain a) where
  append (MarkovChain ss ts) (MarkovChain ss' ts') = (MarkovChain (ss ++ ss') (ts ++ ts'))

instance monoidMarkovChain :: Monoid (MarkovChain a) where
  mempty = MarkovChain Nil Nil

fromState :: forall a. State a -> a
fromState (State a) = a