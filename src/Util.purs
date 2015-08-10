module Util where

import Prelude

import Data.Maybe
import qualified Data.Maybe.Unsafe as U
import qualified Data.Array as A
import qualified Data.String as S
import Data.List
import Data.Int
import Data.Tuple
import Data.Time

import Control.Monad.Eff
import Control.Monad.Eff.Random

-- | find the first element in a list with a property. Useful if the list is increasing or decreasing.
mu :: forall a. (a -> Boolean) -> List a -> Maybe a
mu pred xs = head $ filter pred xs

mu' :: forall a. (a -> Boolean) -> List a -> Maybe a
mu' pred xs = last $ filter pred xs

-- | given a list of integers, normalize it so the largest element is 1
normalize :: List Int -> List Number
normalize xs = map (/ (toNumber $ length xs)) $ map toNumber xs

choose :: forall a e. List a -> Eff ( random :: RANDOM | e) (Maybe a)
choose Nil = return Nothing
choose xs = do
  let n = length xs
      partition = zip (normalize (1 .. n)) xs
  i <- random
  maybe (return $ head xs) (return <<< Just <<< snd) $ mu (\ pair -> i < fst pair) partition

unsafeChoose :: forall a e. List a -> Eff ( random :: RANDOM | e) a
unsafeChoose xs = U.fromJust <$> choose xs

-- | given a list of elements, split it up into a list of overlapping (if k > 1) sublists of length k
kgram :: forall a. Int -> List a -> List (List a)
kgram _ Nil = Nil
kgram n lst = go lst
  where
    go Nil = Nil
    go xs@(Cons x rest)
      | length xs < n = (append xs $ take (n - length xs) lst) : go rest
      | otherwise = take n xs : go rest

cut :: forall a e. Array a -> Eff ( random :: RANDOM | e ) (Array a)
cut xs = do
  i <- randomInt 0 $ A.length xs - 1
  return $ (U.fromJust $ xs A.!! i) A.: A.take i xs ++ A.drop (i + 1) xs

runEpochMilliseconds :: Milliseconds -> Int
runEpochMilliseconds (Milliseconds n) = n

alphabetize :: String -> List String
alphabetize = toList <<< S.split "" -- <<< S.joinWith " " <<< S.split "\n"

unNLTextFile = S.joinWith "" <<< S.split "\n"
