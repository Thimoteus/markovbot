module IRC where

import Prelude
import Markov
import Util
import Types

import Data.List hiding (insert)
import Data.Foldable
import Data.Traversable
import Data.Date
import qualified Data.List.Unsafe as UL
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

instance nickEq :: Eq Nick where
  eq (Nick x) (Nick y) = x == y

nickServ = Channel "NickServ"

getMessage :: forall e. Int -> Array (MarkovChain (List String)) -> Eff ( random :: RANDOM | e ) String
getMessage m chains = do
  chain <- unsafeChoose $ toList chains
  path <- createPath m chain ((== "\n") <<< UL.last) 0.8
  return $ showPathOfStrings path

containsKeywords :: String -> Array String -> Boolean
containsKeywords str keywords = or $ S.contains <$> keywords <*> [str]

lmrHandler :: forall e. BotInfo -> Array (MarkovChain (List String)) -> Aff (irc :: IRC, console :: CONSOLE | e) Unit
lmrHandler bot chains = connect bot.host bot.nick bot.chan $ do
  timer <- liftEff $ newSTRef 0
  sayChannel nickServ $ MessageText $ "identify " ++ bot.password
  onChannelMessage bot.chan \ event -> do
    now <- liftEff $ runEpochMilliseconds <$> nowEpochMilliseconds
    before <- liftEff $ readSTRef timer
    let text = runMessageText event.text
        nick = runNick event.nick
    when (containsKeywords text bot.keywords && now - before >= 10000) $ do
      msg <- liftEff $ getMessage 400 chains
      liftEff $ modifySTRef timer (const now)
      sayChannel bot.chan $ MessageText $ nick ++ ": " ++ msg

lmrSimHandler :: forall e. BotInfo -> Array (MarkovChain (List String)) -> Aff (irc :: IRC, console :: CONSOLE | e) Unit
lmrSimHandler bot chains = connect bot.host bot.nick bot.chan $ do
  sayChannel nickServ $ MessageText $ "identify " ++ bot.password
  onChannelMessage bot.chan \ event -> do
    let text = runMessageText event.text
        sender = event.nick
    i <- liftEff $ random
    when (bot.nick /= sender && i < 0.9) $ do
      msg <- liftEff $ getMessage 400 chains
      sayChannel bot.chan $ MessageText msg

--runBot :: forall e. String -> Array Channel -> Setup e Unit -> _
runBot :: BotInfo -> _
runBot bot = do
  filenames <- map (("data/" ++ bot.name ++ "/") ++) <<< A.filter (S.contains bot.name) <$> readdir ("data/" ++ bot.name ++ "/")
  txts <- sequence $ readTextFile UTF8 <$> filenames
  let alph = map alphabetize txts
      chains = mkMarkovChain 5 <$> alph

  launchAff $ lmrHandler bot chains
