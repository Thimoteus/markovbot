module Distiller where

import Prelude

import qualified Data.String as S
import qualified Parser as P
import Data.Array
import Types

import Node.FS.Sync
import Node.Encoding

import Control.Monad.Eff.Console

main = do
  alphabetTxt <- readTextFile UTF8 "data/in.txt"
  name <- S.joinWith "" <<< S.split "\n" <$> readTextFile UTF8 "data/name"
  let alphabet = S.joinWith "\n" $ map (S.drop 34) $ filter (P.parsableWith (\ msg -> msg.name == name)) $ S.split "\n" alphabetTxt
  writeTextFile UTF8 "data/out.txt" alphabet
