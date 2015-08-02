module Markov where

import Prelude
import S2DBot.Types

import Data.List
import Data.Int
import Data.Maybe
import Data.Tuple

import Control.MonadPlus
import Control.Monad.Eff
import Control.Monad.Eff.Random

states :: forall a. MarkovChain a -> List (State a)
states (MarkovChain s _) = s

transitions :: forall a. MarkovChain a -> List (Transition a)
transitions (MarkovChain _ t) = t

source :: forall a. Transition a -> State a
source (Transition s _) = s

dest :: forall a. Transition a -> State a
dest (Transition _ d) = d

possibleTransitions :: forall a. (Eq a) => MarkovChain a -> State a -> List (Transition a)
possibleTransitions chain curr = do
  possibility <- transitions chain
  guard $ curr == source possibility
  return possibility

addTransition :: forall a. MarkovChain a -> Transition a -> MarkovChain a
addTransition (MarkovChain states transs) trans = MarkovChain states (trans : transs)

normalize :: List Int -> List Number
normalize xs = map (/ (toNumber $ length xs)) $ map toNumber xs

mu :: forall a. (a -> Boolean) -> List a -> Maybe a
mu pred xs = last $ filter pred xs

nextState :: forall a e. (Eq a) => MarkovChain a -> State a -> Eff ( random :: RANDOM | e ) (State a)
nextState chain curr = do
  let pt = possibleTransitions chain curr
      n = length pt
      partition = zip (normalize (1 .. n)) pt
  p <- random
  maybe (return Start) (return <<< dest <<< snd) $ mu (\ pair -> fst pair < p) partition
