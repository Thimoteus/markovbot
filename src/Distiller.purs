module Distiller where

import Prelude

import qualified Data.String as S
import qualified Parser as P
import Data.Array
import S2DBot.Types

import Node.FS.Sync
import Node.Encoding

import Control.Monad.Eff.Console

isShaggy :: IRCMessage -> Boolean
isShaggy msg = msg.name == "ShaggyTwoDope"

main = do
  alphabetTxt <- readTextFile UTF8 "data/in.txt"
  let alphabet = S.joinWith "\n" $ filter (P.parsableWith isShaggy) $ S.split "\n" alphabetTxt
  writeTextFile UTF8 "data/me.txt" alphabet
