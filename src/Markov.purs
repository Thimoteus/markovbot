module Markov where

import Prelude
import S2DBot.Types

import Data.List
import Data.Int
import Data.Maybe
import Data.Tuple
import qualified Data.String as S
import qualified Data.List.Unsafe as U

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

start :: forall a. MarkovChain a -> State a
start (MarkovChain states _) = U.head states

possibleTransitions :: forall a. (Eq a) => MarkovChain a -> State a -> List (Transition a)
possibleTransitions chain curr = do
  possibility <- transitions chain
  guard $ curr == source possibility
  return possibility

addTransition :: forall a. MarkovChain a -> Transition a -> MarkovChain a
addTransition (MarkovChain states transs) trans = MarkovChain states (trans : transs)

addTransitions :: forall a. MarkovChain a -> Transitions a -> MarkovChain a
addTransitions (MarkovChain states transs) transs' = MarkovChain states (transs ++ transs')

addState :: forall a. MarkovChain a -> State a -> MarkovChain a
addState (MarkovChain states transs) state = MarkovChain (state : states) transs

addStates :: forall a. MarkovChain a -> States a -> MarkovChain a
addStates (MarkovChain states transs) states' = MarkovChain (states ++ states') transs

states2transs :: forall a. States a -> Transitions a
states2transs Nil = Nil
states2transs (Cons s Nil) = return $ Transition s s
states2transs states = cycle : chain
  where
    cycle = Transition (U.last states) (U.head states)
    chain = zipWith Transition states $ drop 1 states

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
  maybe (return $ start chain) (return <<< dest <<< snd) $ mu (\ pair -> fst pair < p) partition

emptyChain :: forall a. MarkovChain a
emptyChain = MarkovChain Nil Nil

kgram :: forall a. Int -> List a -> List (List a)
kgram _ Nil = Nil
kgram n lst@(Cons x xs)
  | n > length xs + 1 = Nil
  | otherwise = take n lst : kgram n xs

mkStates :: forall a. Int -> List a -> States (List a)
mkStates n lst = map State $ kgram n lst

