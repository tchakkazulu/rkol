module Main where

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Chan
import Control.Monad (when, msum)
import Data.Char (isSpace)
import Data.List (isInfixOf)

import System.Environment (getArgs)

import System.Directory (doesFileExist)

import IRCBot
import IRCBot.Whois

import Config

import Util

import Poll
import Formatter
import Shoutcast.Fetch
import Shoutcast.Formatter
import Shoutcast.ServerStats

rkolConfig :: IRCDetails -> IrcConfig
rkolConfig det = defaultConfig { cAddr = ircserver det
                               , cPort = ircport det
                               , cNick = ircnick det
                               , cUsername = ircuser det
                               , cRealname = ircreal det
                               , cChannels = ircchannels det
                               , cCTCPVersion = "tchatbot v2 by tchakkazulu"
                               }

data IRCState = IS { owner :: String
                   , live :: Bool
                   , quitter :: String -> Bot IRCState ()
                   , formatter :: [Maybe ServerStats] -> String
                   , song :: [Maybe ServerStats]
                   }

initialIS :: IRCState
initialIS = IS "" True disconnect (const "") []

quit :: String -> Bot IRCState ()
quit msg = getProp quitter >>= ($msg)

main :: IO ()
main = do
  args <- getArgs
  let configFile = case args of
                     [c] -> c
                     _   -> "rkol.cfg"
  config <- (getDataFileName' configFile >>= configFromFile)
  fmt <- (getDataFileName' (rformat . cfgradio $ config) >>= formatterFromFile)
  (ch, tid) <- pollChanWith cmpStats (songGetter . rurls . cfgradio $ config) (10*1000*1000)
  -- This will get stuck in cmdLoop.
  let cfg = rkolConfig $ cfgirc config
      is = initialIS{formatter = fmt}
  startBot (\q -> attachEvents q >> forkChatter ch >>= cmdLoop) cfg is
  killThread tid
  putStrLn "Done?"

songGetter :: [ServerURL] -> IO [Maybe ServerStats]
songGetter = mapM getPage

cmpStats :: [Maybe ServerStats] -> [Maybe ServerStats] -> Bool
cmpStats old new = not ("rkol" `isInfixOf` fn) && (f old /= fn)
  where f = maybe "" curSong . msum
        fn = f new


forkChatter :: Chan [Maybe ServerStats] -> Bot IRCState ThreadId
forkChatter ch = forkBot $ go
  where go = do
          stats <- liftIO $ readChan ch
          fmt <- getProp formatter
          let songInfo = fmt stats
          modVar $ \st -> st{song = stats}
          liftIO $ putStrLn songInfo
          sendMsg $ Message Nothing "NOTICE" ["#tchakkablaat", songInfo]
          go

attachEvents :: (String -> Bot IRCState ()) -> Bot IRCState ()
attachEvents myquit = do
  let myquit' msg = do
        modVar $ \st -> st{live = False}
        myquit msg
  modVar $ \st -> st{quitter = myquit'}
  addEvent EPrivmsg quitFromIrc
  addEvent EPrivmsg ircSong
  addEvent ENick handleNick
  return ()

ircSong :: Message -> Bot IRCState ()
ircSong (Message (Just (NickName nick _ _)) "PRIVMSG" [aimedAt,text]) = do
  let (cmd, rest) = break isSpace text
  when (cmd == "!song") $ do
    let to = case aimedAt of
               ('#':_) -> aimedAt
               _ -> nick
    s <- getProp song
    fmt <- getProp formatter
    sendMsg $ Message Nothing "NOTICE" [to, fmt s]

quitFromIrc :: Message -> Bot IRCState ()
quitFromIrc (Message (Just (NickName nick _ _)) "PRIVMSG" [aimedAt,text]) = do
  me <- getNickname
  when (aimedAt == me) $ do
    let (cmd,rest) = break isSpace text
    when (cmd == "!quit") $ do
      let quitMsg = dropWhile isSpace rest
      own <- getProp owner
      when (nick == own) $ quit quitMsg


handleNick :: Message -> Bot IRCState ()
handleNick (Message (Just (NickName old _ _)) "NICK" [new]) = do
  own <- getProp owner
  when (own == old) $ modVar (\st -> st{owner = new})
handleNick _ = return ()


cmdLoop :: ThreadId -> Bot IRCState ()
cmdLoop tid = go 
 where go = do
         x <- liftIO $ getLine
         p <- getProp live
         when p $ do
           case break isSpace x of
             ("!quit", rest)  -> liftIO (killThread tid) >> quit (dropWhile isSpace rest)
             ("!owner", rest) -> modVar (\st -> st{owner = dropWhile isSpace rest})
             ("!whois", rest) -> performWhois (dropWhile isSpace rest) (liftIO . print)
             ("!raw", rest)   -> sendRaw (dropWhile isSpace rest)
             ("!fmt", rest)   -> changeFormat (dropWhile isSpace rest)
             ("!song", rest)  -> consoleSong
             _ -> return ()
           go

changeFormat :: FilePath -> Bot IRCState ()
changeFormat fmtFile = do
  fmtFile' <- liftIO $ getDataFileName' fmtFile
  p <- liftIO $ doesFileExist fmtFile'
  if not p then liftIO (putStrLn $ "File " ++ fmtFile' ++ " not found.") else do
    fmt <- liftIO $ formatterFromFile fmtFile'
    modVar $ \st -> st{formatter = fmt}

consoleSong :: Bot IRCState ()
consoleSong = do
  s <- getProp song
  fmt <- getProp formatter
  liftIO $ putStrLn (fmt s)
