module Poll where

import Control.Monad (when)

import Control.Concurrent
import Control.Concurrent.MVar

-- | Given an `action` and a `delay`, perform `action` every `delay` microseconds.
-- The returned `MVar a` contains the most recent result of the action.
-- The returned `ThreadId` is the ThreadId of the polling thread.
poll :: IO a -> Int -> IO (MVar a, ThreadId)
poll action delay = do
    var <- newEmptyMVar
    tid <- forkIO (action >>= putMVar var >> loop var)
    return (var, tid)
  where loop var = go
          where go = do
                  threadDelay delay
                  action >>= swapMVar var
                  go

-- | Given a a comparison `cmp`, `action` and a `delay`, perform `action` every `delay` microseconds.
-- The returned `Chan a` contains consecutive results, whenever `cmp` `old` `new` is `True` of the action.
-- The returned `ThreadId` is the `ThreadId` of the polling thread.
pollChanWith :: (a -> a -> Bool) -> IO a -> Int -> IO (Chan a, ThreadId)
pollChanWith cmp action delay = do
  ch <- newChan
  tid <- forkIO $ do
    x <- action
    writeChan ch x
    loop ch x
  return (ch, tid)
 where loop ch = go
         where go old = do
                 threadDelay delay
                 new <- action
                 if (cmp old new) then writeChan ch new >> go new else go old
