module Conc where

import Control.Concurrent
import Control.Exception hiding (catch)
import System.IO.Unsafe

-- Special threading subsystem. Makes stuff work.
children :: MVar [(ThreadId, MVar ())]
children = unsafePerformIO (newMVar [])
    
waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
       putMVar children ms
       takeMVar (snd m)
       waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    tid <- forkIO (io `finally` putMVar mvar () >> killAllChildren)
    putMVar children ((tid,mvar):childs)
    return tid

killAllChildren :: IO ()
killAllChildren = do
  cs <- takeMVar children
  mapM_ (killThread . fst) cs
-- End of threading subsystem.

