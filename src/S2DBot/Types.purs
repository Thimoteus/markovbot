module S2DBot.Types where

import Prelude

import Data.List
import Data.Tuple

import Control.MonadPlus
import Control.Monad.Eff.Random

type IRCMessage = { name :: String, message :: String }

data State a = State a | Start
data Transition a = Transition (State a) (State a)
data MarkovChain a = MarkovChain (List (State a)) (List (Transition a))

instance stateEq :: (Eq a) => Eq (State a) where
  eq Start Start = true
  eq (State x) (State y) = x == y
  eq _ _ = false

instance transitionEq :: (Eq a) => Eq (Transition a) where
  eq (Transition s d) (Transition s' d') = s == s' && d == d'

instance markovchainEq :: (Eq a) => Eq (MarkovChain a) where
  eq (MarkovChain ss ts) (MarkovChain ss' ts') = ss == ss' && ts == ts'

instance showState :: (Show a) => Show (State a) where
  show Start = "Start"
  show (State a) = "State " ++ show a

instance showTransition :: (Show a) => Show (Transition a) where
  show (Transition s d) = show s ++ " -> " ++ show d

instance showMarkovChain :: (Show a) => Show (MarkovChain a) where
  show (MarkovChain states transitions) = "MarkovChain {" ++ showList states ++ "}, {" ++ showList transitions ++ "}"

showList :: forall a. (Show a) => List a -> String
showList Nil = ""
showList (Cons x Nil) = show x
showList (Cons x xs) = show x ++ ", " ++ showList xs
