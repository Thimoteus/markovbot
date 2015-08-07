module IRC where

import Prelude
import Markov
import Util
import Types

import Data.List hiding (insert)
import Data.Traversable
import Data.Date
import qualified Data.String as S
import qualified Data.Array as A
import qualified Data.Array.Unsafe as UA

import Control.Monad
import Control.Monad.ST
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import Control.Monad.Aff
import Control.MonadPlus

import Node.IRC
import Node.FS.Sync
import Node.Encoding

chan = Channel "#linuxmasterrace"
nickServ = Channel "NickServ"

getMessage :: forall e. Int -> Array (MarkovChain (List String)) -> Eff ( random :: RANDOM | e ) String
getMessage m chains = do
  chain <- unsafeChoose $ toList chains
  path <- createPath m chain
  return $ showPathOfStrings path

runBot :: String -> _
runBot str = do
  filenames <- map ("data/" ++) <<< A.filter (S.contains "shag") <$> readdir "data/"
  txts <- sequence $ readTextFile UTF8 <$> filenames
  let alph = map alphabetize txts
      chains = mkMarkovChain 5 <$> alph
  password <- S.joinWith "" <<< S.split "\n" <$> readTextFile UTF8 "data/password"
  
  launchAff $ do
    connect (Host "irc.snoonet.org") (Nick "shaggytwobot") chan $ do
      timer <- liftEff $ newSTRef 0
      sayChannel nickServ $ MessageText $ "identify " ++ password

      onChannelMessage chan \ event -> do
        now <- liftEff $ runEpochMilliseconds <$> nowEpochMilliseconds
        before <- liftEff $ readSTRef timer
        let text = runMessageText event.text
            nick = runNick event.nick
        when (S.contains "shaggytwobot" text && now - before >= 10000) $ do
          msg <- liftEff $ getMessage 400 chains
          liftEff $ modifySTRef timer (const now)
          sayChannel chan $ MessageText $ nick ++ ": " ++ msg
