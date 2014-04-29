{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IRCBot ( startBot
              , getProp
              , modVar
              , performCall
              , addEvent
              , sendRaw
              , sendMsg
              , forkBot
              , EvType(..)
              , MSG.Message(..)
              , MSG.Prefix(..)
              , IRC.IrcConfig(..)
              , IRC.defaultConfig
              , liftIO
              , Data.Unique.Unique
              , remEvent
              , disconnect
              , reconnect
              , getNickname
              , Bot
              ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader

import Data.Unique (Unique)

import Data.Char (isSpace)
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

import qualified Network.SimpleIRC as IRC
import Network.SimpleIRC (MIrc)
import qualified Network.IRC as MSG

newtype Bot s a = Bot {unBot :: ReaderT (MVar s, MIrc) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runBot :: Bot s a -> (MVar s, MIrc) -> IO a
runBot = runReaderT . unBot

bot :: ((MVar s, MIrc) -> IO a) -> Bot s a
bot = Bot . ReaderT

withMIrc :: (MIrc -> IO a) -> Bot s a
withMIrc f = bot (\(_,mirc) -> f mirc)

botState :: Bot s (MVar s, MIrc)
botState = Bot ask

getProp :: (s -> a) -> Bot s a
getProp f = do
  (is,_) <- botState
  is' <- liftIO $ readMVar is
  return (f is')

modVar :: (s -> s) -> Bot s ()
modVar f = botState >>= liftIO . flip modifyMVar_ (return . f) . fst

performCall :: a -> MSG.Message -> [(EvType, MSG.Message -> a -> a)] -> EvType -> (a -> Bot s ()) -> Bot s ()
performCall emptyR msg typs stoptyp f = do
  cr <- liftIO $ newMVar emptyR
  uniqs <- forM typs $ \(typ,fill) -> addEvent typ (\msg -> liftIO $ modifyMVar_ cr (\st -> return (fill msg st)))
  thing <- liftIO $ newEmptyMVar
  uniqStops <- addEvent stoptyp (stopHdl cr uniqs thing)
  liftIO $ putMVar thing uniqStops
  sendMsg msg
 where stopHdl cr uniqs thing msg = do
         (_,mirc) <- botState
         liftIO $ forM uniqs $ IRC.remEvent mirc
         self <- liftIO $ readMVar thing
         liftIO $ IRC.remEvent mirc self
         cr' <- liftIO $ readMVar cr
         f cr'

sendRaw :: String -> Bot s ()
sendRaw msg = bot $ \(_,mirc) -> IRC.sendRaw mirc (BS.pack msg)

sendMsg :: MSG.Message -> Bot s ()
sendMsg = sendRaw . MSG.encode

addEvent :: EvType -> (MSG.Message -> Bot s ()) -> Bot s Unique
addEvent typ handle = bot $ \(is,mirc) -> IRC.addEvent mirc . (toEvent typ) $ (\mirc' msg -> runBot (handle (reparse msg)) (is,mirc'))

reparse :: IRC.IrcMessage -> MSG.Message
reparse = fromJust . MSG.decode . (++ "\r\n") . BS.unpack . IRC.mRaw

data EvType = EPrivmsg
            | ENum Int
            | EPing
            | EJoin
            | EPart
            | EMode
            | ETopic
            | EInvite
            | EKick
            | EQuit
            | ENick
            | ENotice
            | ERaw
            | EDisconnect

toEvent :: EvType -> IRC.EventFunc -> IRC.IrcEvent
toEvent EPrivmsg = IRC.Privmsg
toEvent (ENum i) = IRC.Numeric . acceptOnlyNum i
  where acceptOnlyNum i f mirc msg | MSG.msg_command msg' == show i = f mirc msg
                                   | otherwise = return ()
          where msg' = reparse msg
toEvent EPing = IRC.Ping
toEvent EJoin = IRC.Join
toEvent EPart = IRC.Part
toEvent EMode = IRC.Mode
toEvent ETopic = IRC.Topic
toEvent EInvite = IRC.Invite
toEvent EKick = IRC.Kick
toEvent EQuit = IRC.Quit
toEvent ENick = IRC.Nick
toEvent ENotice = IRC.Notice
toEvent ERaw = IRC.RawMsg
toEvent EDisconnect = IRC.Disconnect . fillMsg
  where fillMsg eventfunc = \mirc -> eventfunc mirc (IRC.IrcMessage Nothing Nothing Nothing Nothing BS.empty BS.empty Nothing Nothing Nothing BS.empty)

startBot :: ((String -> Bot s ()) -> Bot s ()) -> IRC.IrcConfig -> s -> IO ()
startBot bot cfg is' = do
  mirc' <- IRC.connect cfg True False
  case mirc' of
    Left errs -> print errs
    Right mirc -> do
      is <- newMVar is'
      runBot (getQuitter >>= bot) (is, mirc)
      return ()
 where getQuitter = do
         quitH <- addEvent EDisconnect (const $ reconnect' 3)
         let myquit quitMsg = do
               remEvent quitH
               disconnect quitMsg
               liftIO $ threadDelay (3*1000*1000)
         return myquit
       reconnect' 0 = return ()
       reconnect' n = liftIO (threadDelay (5*1000*1000)) >> do
         (_, mirc) <- botState
         x <- liftIO $ IRC.reconnect mirc
         case x of
           Left errs  -> liftIO (print errs) >> reconnect' (n-1)
           Right mirc -> return ()

remEvent :: Unique -> Bot s ()
remEvent ev = bot (\(_,mirc) -> IRC.remEvent mirc ev)

disconnect :: String -> Bot s ()
disconnect msg = bot (\(_,mirc) -> IRC.disconnect mirc (BS.pack msg))

reconnect :: Bot s (Either IOError MIrc)
reconnect = bot (\(_,mirc) -> IRC.reconnect mirc)

getNickname :: Bot s String
getNickname = fmap BS.unpack $ withMIrc IRC.getNickname

forkBot :: Bot s () -> Bot s ThreadId
forkBot b = bot (forkIO . runBot b)
