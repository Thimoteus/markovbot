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

data State a = State a | Start a
type States a = V.Set (State a)
type Transitions a = M.Map (State a) (List (State a))
data MarkovChain a = MarkovChain (States a) (Transitions a)

instance stateEq :: (Eq a) => Eq (State a) where
  eq (State x) (State y) = x == y
  eq (Start x) (Start y) = x == y
  eq _ _ = false

instance markovchainEq :: (Eq a) => Eq (MarkovChain a) where
  eq (MarkovChain ss ts) (MarkovChain ss' ts') = ss == ss' && ts == ts'

instance stateShow :: (Show a) => Show (State a) where
  show (State s) = "State " ++ show s
  show (Start s) = "Start " ++ show s

instance markovChainShow :: (Show a) => Show (MarkovChain a) where
  show (MarkovChain states transitions) = "MarkovChain {" ++ showSet states ++ "}, {" ++ showMap transitions ++ "}"

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

instance ordState :: (Ord a) => Ord (State a) where
  compare (State s) (State s') = compare s s'
  compare (Start s) (Start s') = compare s s'
  compare (Start _) (State _) = LT
  compare (State _) (Start _) = GT

instance functorState :: Functor State where
  map f (State s) = State (f s)
  map f (Start s) = Start (f s)

instance semigroupMarkovChain :: (Ord a) => Semigroup (MarkovChain a) where
  append (MarkovChain ss ts) (MarkovChain ss' ts') = MarkovChain (ss ++ ss') (ts ++ ts')

instance monoidMarkovChain :: (Ord a) => Monoid (MarkovChain a) where
  mempty = MarkovChain V.empty M.empty

fromState :: forall a. State a -> a
fromState (State a) = a
fromState (Start a) = a

showMap :: forall k v. (Show k, Show v) => M.Map k (List v) -> String
showMap = ("Map " ++) <<< showList <<< map (showList <<< snd) <<< M.toList

showSet :: forall e. (Show e) => V.Set e -> String
showSet = ("Set " ++) <<< showList <<< V.toList

mapStates :: forall a b. (Ord a, Ord b) => (a -> b) -> States a -> States b
mapStates f ss = V.toList ss # map (map f) # V.fromList

mapTransition :: forall a b. (Ord a, Ord b) => (a -> b) -> M.Map a (List a) -> M.Map b (List b)
mapTransition f dict = M.toList dict # map (mapTuple f) # M.fromList
  where
    mapTuple f (Tuple x y) = Tuple (f x) (map f y)

mapMarkovChain :: forall a b. (Ord a, Ord b) => (a -> b) -> MarkovChain a -> MarkovChain b
mapMarkovChain f (MarkovChain ss ts) = MarkovChain (mapStates f ss) (mapTransition (map f) ts)
