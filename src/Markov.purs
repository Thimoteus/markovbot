module Markov where

import Prelude
import Types

import Data.List
import Data.Int
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Either
import qualified Data.String as S
import qualified Data.List.Unsafe as U

import Control.MonadPlus
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Random
import Control.Monad.Rec.Class
import Control.Monad.Eff.Console

-- | get the states from a MarkovChain
states :: forall a. MarkovChain a -> List (State a)
states (MarkovChain s _) = s

-- | get the transitions from a MarkovChain
transitions :: forall a. MarkovChain a -> List (Transition a)
transitions (MarkovChain _ t) = t

-- | get the source state for a transition
source :: forall a. Transition a -> State a
source (Transition s _) = s

-- | get the destination state for a transition
dest :: forall a. Transition a -> State a
dest (Transition _ d) = d

-- | get the first state in a MarkovChain
start :: forall a. MarkovChain a -> State a
start (MarkovChain states _) = U.head states

-- | given a MarkovChain and a state in it, return a list of transitions in which that state is the source
possibleTransitions :: forall a. (Eq a) => MarkovChain a -> State a -> List (Transition a)
possibleTransitions chain curr = do
  possibility <- transitions chain
  guard $ curr == source possibility
  return possibility

-- | add a single transition to a MarkovChain
addTransition :: forall a. MarkovChain a -> Transition a -> MarkovChain a
addTransition (MarkovChain states transs) trans = MarkovChain states (trans : transs)

-- | add a list of transitions to a MarkovChain
addTransitions :: forall a. MarkovChain a -> Transitions a -> MarkovChain a
addTransitions (MarkovChain states transs) transs' = MarkovChain states (transs ++ transs')

-- | add a single state to a MarkovChain
addState :: forall a. MarkovChain a -> State a -> MarkovChain a
addState (MarkovChain states transs) state = MarkovChain (state : states) transs

-- | add a list of states to a MarkovChain
addStates :: forall a. MarkovChain a -> States a -> MarkovChain a
addStates (MarkovChain states transs) states' = MarkovChain (states ++ states') transs

-- | generate a list of transitions from a list of states
states2transs :: forall a. States a -> Transitions a
states2transs Nil = Nil
states2transs (Cons s Nil) = return $ Transition s s
states2transs states = cycle : chain
  where
    cycle = Transition (U.last states) (U.head states)
    chain = zipWith Transition states $ drop 1 states

-- | given a list of integers, normalize it so the largest element is 1
normalize :: List Int -> List Number
normalize xs = map (/ (toNumber $ length xs)) $ map toNumber xs

-- | find the first element in a list with a property. Useful if the list is increasing or decreasing.
mu :: forall a. (a -> Boolean) -> List a -> Maybe a
mu pred xs = head $ filter pred xs

-- | given a MarkovChain and a state, return uniformly at random another state accessible from the first
nextState :: forall a e. (Eq a) => MarkovChain a -> State a -> Eff ( random :: RANDOM | e ) (State a)
nextState chain curr = do
  maybeState <- choose $ possibleTransitions chain curr
  maybe (return $ start chain) (return <<< dest) maybeState

-- | the empty MarkovChain
emptyChain :: forall a. MarkovChain a
emptyChain = MarkovChain Nil Nil

-- | given a list of elements, split it up into a list of overlapping (if k > 1) sublists of length k
kgram :: forall a. Int -> List a -> List (List a)
kgram _ Nil = Nil
kgram n lst = go lst
  where
    go Nil = Nil
    go xs@(Cons x rest)
      | length xs < n = (append xs $ take (n - length xs) lst) : go rest
      | otherwise = take n xs : go rest

-- | given a k and a list of elements, create states of k-grams
mkStates :: forall a. Int -> List a -> States (List a)
mkStates n lst = map State $ kgram n lst

-- | forge a path of length n through a given MarkovChain starting with the first state
createPath :: forall a e. (Eq a) => Int -> MarkovChain a -> Eff ( random :: RANDOM | e ) (States a)
createPath n chain = tailRecM createPath' { count: n, lst: singleton $ start chain }
  where
    createPath' { count = 0 , lst = acc } = return $ Right acc
    createPath' { count = k, lst = acc@(Cons top _) } = do
      next <- nextState chain top
      return $ Left { count: k - 1, lst: next : acc }

showPath :: forall a. (Show a) => States (List a) -> String
showPath = S.joinWith "" <<< fromList <<< reverse <<< map (\ (State x) -> show $ U.last x)

showPathOfStrings :: States (List String) -> String
showPathOfStrings = S.joinWith "" <<< fromList <<< extractStrings <<< reverse
  where
    extractStrings :: States (List String) -> _
    extractStrings (Cons x xs) = (S.joinWith "" $ fromList $ fromState x) : (map (U.last <<< fromState) xs)

-- | These functions always exists when the input is finite
choose :: forall a e. List a -> Eff ( random :: RANDOM | e ) (Maybe a)
choose Nil = return Nothing
choose xs = do
  let n = length xs
      partition = zip (normalize (1 .. n)) xs
  i <- random
  maybe (return $ head xs) (return <<< Just <<< snd) $ mu (\ pair -> i < fst pair) partition

showState :: State (List String) -> String
showState (State xs) = "State " ++ showListString xs

showTransition :: Transition (List String) -> String
showTransition (Transition s d) = showState s ++ " -> " ++ showState d

showStates :: States (List String) -> String
showStates = showListString <<< map showState

showTransitions :: Transitions (List String) -> String
showTransitions = showListString <<< map showTransition

showStringChain :: MarkovChain (List String) -> String
showStringChain (MarkovChain states transitions) = "MarkovChain {" ++ showStates states ++ "}, {" ++ showTransitions transitions ++ "}"

