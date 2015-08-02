module S2DBot.Types where

import Prelude

import Data.List
import Data.Tuple

import Control.MonadPlus
import Control.Monad.Eff.Random

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

instance showState :: (Show a) => Show (State a) where
  show (State s) = "State " ++ show s

instance showTransition :: (Show a) => Show (Transition a) where
  show (Transition s d) = show s ++ " -> " ++ show d

instance showMarkovChain :: (Show a) => Show (MarkovChain a) where
  show (MarkovChain states transitions) = "MarkovChain {" ++ showList states ++ "}, {" ++ showList transitions ++ "}"

showList :: forall a. (Show a) => List a -> String
showList Nil = ""
showList (Cons x Nil) = show x
showList (Cons x xs) = show x ++ ", " ++ showList xs

instance functorTransition :: Functor Transition where
  map f (Transition (State s) (State d)) = Transition (State (f s)) (State (f d))

instance functorState :: Functor State where
  map f (State s) = State (f s)

instance functorMarkovChain :: Functor MarkovChain where
  map f (MarkovChain states transs) = MarkovChain (map (map f) states) (map (map f) transs)

instance semigroupMarkovChain :: Semigroup (MarkovChain a) where
  append (MarkovChain ss ts) (MarkovChain ss' ts') = (MarkovChain (ss ++ ss') (ts ++ ts'))

