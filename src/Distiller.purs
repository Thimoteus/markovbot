module Distiller where

import Prelude
import Types
import Util

import qualified Data.String as S
import qualified Parser as P
import Data.Array
import Data.Function (on)

import Node.FS.Sync
import Node.Encoding

import Control.Monad
import Control.Monad.Eff.Console

goodAuthor :: String -> IRCMessage -> Boolean
goodAuthor name o = (S.contains `on` S.toLower) name o.name

main = do
  alphabetTxt <- readTextFile UTF8 "data/in.txt"
  name <- unNLTextFile <$> readTextFile UTF8 "data/name"
  let alphabet = S.joinWith "\n" $ map _.message $ filter (goodAuthor name) $ mapMaybe P.readLine $ S.split "\n" alphabetTxt
      dataDir = "data/" ++ name ++ "/"
  dirExists <- exists dataDir
  when (not dirExists) $ mkdir dataDir
  writeTextFile UTF8 ("data/" ++ name ++ "/out") alphabet
