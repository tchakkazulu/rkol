{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot where

import Conc

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception hiding (catch)
import Control.Monad.Reader
-- import Control.Monad.Maybe

import Network.IRC as IRC

-- Smart constructor
mkMessage :: String -> [String] -> IRC.Message
mkMessage = IRC.Message Nothing

-- Channel for IRC messages.
type Tube = Chan IRC.Message

-- An action of type 'Bot s a' can make use of IO and send messages to IRC.
newtype Bot s a = Bot ( ReaderT (Tube, MVar s) IO a )
  deriving (Monad, Functor, MonadIO)


runBot :: Bot s a -> Tube -> MVar s -> IO a
runBot (Bot rt) tb st = runReaderT rt (tb,st)

-- Write a message to IRC.
writeMsg :: IRC.Message -> Bot s ()
writeMsg msg = Bot $ do
  ch <- ask
  liftIO $ writeChan (fst ch) msg

forkBot :: Bot s () -> Bot s ThreadId
forkBot bt = Bot $ do
  (ch,st) <- ask
  liftIO $ forkChild (runBot bt ch st >> return ())

getsState :: (s -> a) -> Bot s a
getsState f = Bot $ do
  var <- asks snd
  st <- liftIO $ readMVar var
  return (f st)

modifyState :: (s -> s) -> Bot s s
modifyState f = Bot $ do
  var <- asks snd
  val <- liftIO $ readMVar var
  liftIO $ swapMVar var (f val)

