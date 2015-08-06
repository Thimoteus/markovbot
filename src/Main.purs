module Main where

import Prelude
import Markov
import Util
import Types

import Data.List hiding (insert)
import Data.Traversable
import qualified Data.String as S
import qualified Data.Array as A
import qualified Data.Array.Unsafe as UA

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import Control.Monad.Aff

import Node.IRC
import Node.FS.Sync
import Node.Encoding

alphabetize :: String -> List String
alphabetize = toList <<< S.split "" <<< S.joinWith " " <<< S.split "\n"

chan = Channel "#linuxmasterrace"
nickServ = Channel "NickServ"

getMessage :: forall e. Int -> Int -> Array (MarkovChain (List String)) -> Eff ( random :: RANDOM | e ) String
getMessage m n chains = do
  i <- randomInt m n
  chain <- unsafeChoose $ toList chains
  path <- createPath i chain
  return $ showPathOfStrings path

main = do
  filenames <- map ("data/" ++) <<< A.filter (S.contains "shag") <$> readdir "data/"
  txts <- sequence $ readTextFile UTF8 <$> filenames
  let alph = map alphabetize txts
      chains = mkMarkovChain 7 <$> alph
  paths <- sequence $ createPath 90 <$> chains
  password <- S.joinWith "" <<< S.split "\n" <$> readTextFile UTF8 "data/password"

  launchAff $ do
    connect (Host "irc.snoonet.org") (Nick "shaggytwobot") chan $ do
      sayChannel nickServ $ MessageText $ "identify " ++ password
      sayChannel chan $ MessageText "lol"
      onChannelMessage chan \ event -> do
        let text = runMessageText event.text
            nick = runNick event.nick
        when (S.contains "shaggytwobot" text) $ do
          msg <- liftEff $ getMessage 20 140 chains
          sayChannel chan $ MessageText $ nick ++ ": " ++ msg
