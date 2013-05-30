{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}

module Main where

import Bot
import Conc
import Fetch

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)
import Data.Array as A

import Data.Time.Clock
import Data.Time.Format
import System.Locale

import Prelude hiding (catch)

import Network
import Network.Browser
import Network.HTTP

import qualified Network.IRC as IRC

import Text.Regex.Posix

import System.IO

crlf = "\r\n"

-- Settings
server, nick :: String
chans :: [String]
port :: Integer
server = "irc.synirc.net"
port   = 6667
chans  = ["#radio-kol"]
--chans = ["#tchakkablaat"]
nick   = "tchatbot"

logMe s = do
  time <- getCurrentTime
  appendFile "log.txt" (fmt time ++ " " ++ s ++ "\n")

fmt :: UTCTime -> String
fmt = formatTime defaultTimeLocale "[%d-%m-%Y][%H:%M:%S]"

data ChanState = Normal
               | Voice
               | HalfOp
               | Op
               | Admin
               | Owner
  deriving (Eq, Ord, Show)

data MyState = ST { radios :: Map String Radio
                  , master :: String
                  }

data Radio = Rad { servers :: [Maybe ServerStats]
                 , serverURLs :: [String]
                 , dispMe :: [Maybe ServerStats] -> (ServerStats -> Maybe String) -> String
                 , djFilter :: ServerStats -> Maybe String
                 , lastSong :: String
                 , hasChatbot :: Bool
                 , users :: Map String ChanState
                 }

addRadio name srvs disp = modifyState $ \st -> st{radios = addMe (radios st)}
  where addMe = M.insert name emptyRadio{serverURLs = srvs, dispMe = disp}

modifyStateFor chan f = modifyState $ \st -> st{radios = modMe (radios st)}
  where modMe = M.adjust f chan

getsStateFor chan f = do
  x <- getsState (M.lookup chan . radios)
  case x of
    Nothing -> error $ "getsStateFor " ++ chan
    Just y  -> return (f y)

emptyState :: MyState
emptyState = ST M.empty ""

emptyRadio :: Radio
emptyRadio = Rad [] [] (\_ _ -> "Radio Gaga!") (const Nothing) "" False M.empty


--
-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = forever $ bracket connect disconnect loop
  where
    disconnect (ch,h,st) = hClose h
    loop (ch,h,st) = runBot (run h) ch st >> return ()

--
-- Connect to the server and return the channel to send messages to
--
connect :: IO (Tube, Handle, MVar MyState)
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral Main.port))
    hSetBuffering h NoBuffering
    ch <- newChan
    forkChild $ writer ch h >> hClose h
    var <- newMVar emptyState
    return (ch, h, var)
  where
    notify a = bracket_
        (logMe ("Connecting to " ++ server ++ "...") >> hFlush stdout)
        (logMe "done.")
        a

whileM :: Monad m => m Bool -> m a -> m ()
whileM p act = worker
  where worker = p >>= flip when (act >> worker)

-- Writer thread, listens to the tube, and writes the messages to IRC.
writer :: Tube -> Handle -> IO ()
writer ch h = forever $ do
  msg <- readChan ch
  let s = (IRC.encode msg)
  hPutStr h (s ++ crlf)
  logMe $ "> " ++ s


--
-- We're in the Bot monad now, so we've connected successfully
-- Join a channel, and start processing commands
--
run :: Handle -> Bot MyState ()
run h = do
    writeMsg (IRC.nick nick)
    writeMsg (IRC.user nick "0" "*" "Dutchy van Botmuffin")
    mapM_ (writeMsg . IRC.joinChan) chans
    addRadio "#radio-kol" ["http://209.9.238.5:8794", "http://209.9.238.5:8792"] displayRKoL
--    addRadio "#radiofreerollover" ["http://rfr4life.serverroom.us:8182"] displayRFR
--    addRadio "#tchakkablaat" ["http://209.9.238.5:8792", "http://209.9.238.5:8794"] displayRKoL
    fetchDJMappings "#radio-kol"
    forkBot chatbot
    forkBot inputting
    forkBot (listener h >> liftIO (hClose h >> killAllChildren))
    liftIO $ waitForChildren


inputting :: Bot MyState ()
inputting = forever $ do
  line <- liftIO $ getLine
  case words line of
    (('@':here):rest) -> writeMsg $ IRC.privmsg here (dropWhile isSpace . drop (length here + 1) $ line)
    _                 -> case IRC.decode (line ++ crlf) of
                           Nothing -> return ()
			   Just msg -> writeMsg msg

--
-- Process each line from the server
--
listener :: Handle -> Bot MyState ()
listener h = forever $ do
    s <- liftIO $ init `fmap` hGetLine h
    liftIO $ logMe s
    let msg = fromMaybe (error "Message unreadable") $ IRC.decode (s ++ crlf)
    if ping msg then writeMsg (pong msg) else process msg >>= mapM_ writeMsg
  where
    ping (IRC.Message _ c _) = map toLower c == "ping"
    pong (IRC.Message _ _ a) = mkMessage "PONG" a


changeMappings :: IRC.Message -> MyState -> MyState
changeMappings (IRC.Message _ _ [_,_,chan,l]) st = st{radios = change (radios st)}
  where change = M.adjust (\st -> st{users = M.union (toMap l) (users st)}) chan
        toMap = M.fromList . map toKeyVal . words
        toKeyVal nick@(c:rest) = case c of
                                   '~' -> (rest, Owner)
                                   '&' -> (rest, Admin)
                                   '@' -> (rest, Op)
                                   '%' -> (rest, HalfOp)
                                   '+' -> (rest, Voice)
                                   _   -> (nick, Normal)

-- TODO: needs thorough rewrite.
process :: IRC.Message -> Bot MyState [IRC.Message]
process msg | typeOf msg == "NAMES" || typeOf msg == "353" = modifyState (changeMappings msg) >> return [{-IRC.privmsg "tchakkazulu" (show msg)-}]
            | target msg `notElem` (nick:chans) && False = return []
            | typeOf msg == "NOTICE"
            , source msg == "chatbot" = modifyStateFor "#radio-kol" (\st -> st{hasChatbot = True}) >> silence "#radio-kol"
            | typeOf msg == "MODE" = theMode msg
            | typeOf msg == "QUIT" || typeOf msg == "PART"
            , source msg == "chatbot" = modifyStateFor "#radio-kol" (\st -> st{hasChatbot = False}) >> wakeUp "#radio-kol"
            | typeOf msg == "JOIN"
            , target msg == "#radio-kol"
            , source msg == "chatbot" = modifyStateFor "#radio-kol" (\st -> st{hasChatbot = True}) >> silence "#radio-kol"
            | typeOf msg == "PRIVMSG"
            , source msg == "tchakkazulu"
            , target msg == "tchatbot"
            , "!fetch " `isPrefixOf` content msg = fetchDJMappings msgChan >> return [IRC.privmsg "tchakkazulu" "Job done!"]
            | typeOf msg == "PRIVMSG"
            , target msg == "tchatbot"
            , msgChan `elem` chans
            , "!talk " `isPrefixOf` content msg = auth msgChan Voice (source msg) $ modifyStateFor msgChan (\st -> st{hasChatbot = False}) >> wakeUp msgChan
            | typeOf msg == "PRIVMSG"
            , target msg == "tchatbot"
            , msgChan `elem` chans
            , "!stfu " `isPrefixOf` content msg = auth msgChan Voice (source msg) $ modifyStateFor msgChan (\st -> st{hasChatbot = True}) >> silence msgChan
            | typeOf msg == "PRIVMSG"
            , target msg `elem` chans
            , content msg == "!song" = displayCurrentSong (target msg)
            | typeOf msg == "NICK" = updateNick msg
            | typeOf msg == "PRIVMSG"
            , target msg == nick
            , "!mode " `isPrefixOf` content msg = displayMode (words (content msg))
            | typeOf msg == "PRIVMSG"
            , content msg == "!active" = displayActive (target msg)
            | typeOf msg == "PRIVMSG"
            , "!modes " `isPrefixOf` content msg
            , target msg == nick  = displayModes msgChan
            | otherwise               = return []
  where msgChan = theChan (content msg)

displayModes chan = do
  modes <- getsStateFor chan users
  return [IRC.privmsg "tchakkazulu" (show modes)]

displayActive chan = do
  p <- getsStateFor chan hasChatbot
  if p
    then return [IRC.privmsg chan "I am not updating."]
    else return [IRC.privmsg chan "I am updating."]


theChan str = case words str of
                (a:b:_) -> b
                _       -> "BLEEPBLOOP"

theMode (IRC.Message _ "MODE" [c,this,person]) = theMode' this person c
theMode _ = return []

updateNick msg@(IRC.Message _ "NICK" [newnick]) = do
  forM_ chans $ \chan -> do
    oldState <- getMode chan (source msg)
    let modifier = M.insert newnick oldState . M.delete (source msg)
    modifyStateFor chan (\st -> st{users = modifier (users st)})
  return []

theMode' "+o" = giveMode Op
theMode' "-o" = giveMode Normal
theMode' "+v" = giveMode Voice
theMode' "-v" = giveMode Normal
theMode' "+h" = giveMode HalfOp
theMode' "-h" = giveMode Normal
theMode' "+q" = giveMode Owner
theMode' "-q" = giveMode Normal
theMode' "+a" = giveMode Admin
theMode' "-a" = giveMode Normal
theMode' _    = \_ _ -> return []

giveMode mode person chan = modifyStateFor chan (\st -> st{users = M.insert person mode (users st)}) >> return []

getMode :: String -> String -> Bot MyState ChanState
getMode chan name = do
  stuff <- getsStateFor chan users
  let res = M.lookup name stuff
      res' = case res of
               Nothing    -> Normal
               Just res'' -> res''
  return res'

auth :: String -> ChanState -> String -> Bot MyState [IRC.Message] -> Bot MyState [IRC.Message]
auth chan lvl "tchakkazulu" resp = resp
auth chan lvl person resp = do
  res' <- getMode chan person
  if (res' >= lvl) then resp
                   else return []

displayMode (x:chan:y:rest) | chan `notElem` chans = return [] 
                            | otherwise = do
  stuff <- getsStateFor chan users
  let res = M.lookup y stuff
  return [IRC.privmsg  "tchakkazulu" (show res)]
displayMode (x:chan:stuff) = return [IRC.privmsg "tchakkazulu" (show stuff)]

typeOf, source, target, content :: IRC.Message -> String
typeOf (IRC.Message _ t _) = t
source (IRC.Message (Just (IRC.NickName s _ _)) _ _) = s
source _ = ""
target (IRC.Message _ "PRIVMSG" [t,_]) = t
target (IRC.Message _ "NOTICE" [t,_]) = t
target _ = ""
content (IRC.Message _ "PRIVMSG" [_,c]) = c
content (IRC.Message _ "NOTICE" [_,c]) = c
content _ = ""

silence, wakeUp :: String -> Bot MyState [IRC.Message]
silence chan = do
  p <- getsStateFor chan hasChatbot
  if p then return []
       else do
          modifyStateFor chan (\st -> st{hasChatbot = True})
          return [IRC.privmsg chan "Shutting up now."]
wakeUp chan = do
  p <- getsStateFor chan hasChatbot
  if (not p) then return []
             else do
               modifyStateFor chan (\st -> st{hasChatbot = False})
               return [IRC.privmsg chan "Here we go."]



displayCurrentSong :: String -> Bot MyState [IRC.Message]
displayCurrentSong chan = do
  loadState chan
  getsStateFor chan servers >>= displayInfo chan

fetchDJMappings :: String -> Bot MyState [IRC.Message]
fetchDJMappings "#radio-kol" = do
  file <- liftIO $ lines `fmap` readFile "djmaps.txt"
  let filters = map getFilter . filter (not . null) $ file
  modifyStateFor "#radio-kol" $ \st -> st{djFilter = \stats -> msum (map ($stats) filters)}
  return (length filters `seq` [])
fetchDJMappings _ = return []
  

getFilter :: String -> ServerStats -> Maybe String
getFilter str stats = case matchAllText regex str of
    [match] -> let (matchType, matchThis, result) = ( strip $ fst $ match A.! 1
                                                    , strip $ fst $ match A.! 2
                                                    , strip $ fst $ match A.! 3
                                                    )
                   toMatch = case map toLower matchType of
                               "genre" -> strGenre stats
                               "aim"   -> strAIM stats
                               _       -> "Your Mother"
                   matchExpr = makeRegexOpts compIgnoreCase execBlank matchThis :: Regex
               in if matchTest matchExpr toMatch then Just result else Nothing
    _       -> Nothing
  where regex = makeRegex "([^,]+),([^,]+),([^,]+)" :: Regex
        strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace


loadState :: String -> Bot MyState ()
loadState chan = do
  theServs <- getsStateFor chan serverURLs
  srvs <- liftIO $ do
    mapM (errH . browse . getPage) theServs
  unless (null srvs) $ do
    modifyStateFor chan $ \st -> st{servers = srvs}
    return ()

errH = flip catch (\(SomeException e) -> return Nothing)

chatbot :: Bot MyState ()
chatbot = do
  getsState radios >>= mapM chatbot' . M.keys
  liftIO $ threadDelay 10000000
  chatbot

chatbot' :: String -> Bot MyState ()
chatbot' chan = do
  p <- getsStateFor chan hasChatbot
  unless p $ do
    lastSng <- getsStateFor chan lastSong
    loadState chan
    srvrs <- getsStateFor chan servers
    let theInfo = msum srvrs
    let now = maybe "Servers not available" curSong theInfo
    when (now /= lastSng && not (now =~ "rkol")) $ do
      msgs <- displayInfo chan srvrs
      mapM_ writeMsg msgs
    modifyStateFor chan $ \st -> st{lastSong = now}
    return ()


displayInfo :: String -> [Maybe ServerStats] -> Bot MyState [IRC.Message]
displayInfo chan infos = do
    toDisp <- getsStateFor chan dispMe
    filters <- getsStateFor chan djFilter
    return [mkMessage "NOTICE" [chan,toDisp infos filters]]

-- displayRKoL :: [Maybe ServerStats] -> String
displayRKoL [info64,info24] filters =
    case theInfo of
      Nothing -> "Could not read server information."
      Just _  -> "Radio KoL: " ++ curS ++ " - " ++ text ++ " -" ++ info24' ++ info64'
  where strG = maybe "" strGenre theInfo
        curS = maybe "" curSong theInfo
        strA = maybe "" strAIM theInfo
        info24' = maybe "" (\i -> " 24kbps: " ++ info i) info24
        info64' = maybe "" (\i -> " 64kbps: " ++ info i) info64
        info i = show (curListeners i) ++ "/" ++ show (maxListeners i)
        which = fromMaybe strG . filters $ fromJust theInfo
        theInfo = msum [info64,info24]
        text | null strG     = strA
             | which == strG = which
             | otherwise     = which ++ " - " ++ strG

displayRFR [info] _ =
    case theInfo of
      Nothing -> "Could not read server information."
      Just _  -> "RFR: " ++ curS ++ " - " ++ strG ++ " - " ++ info'
  where strG = maybe "" strGenre theInfo
        curS = maybe "" curSong theInfo
        info' = maybe "" infoo info
        infoo i = show (curListeners i) ++ "/" ++ show (maxListeners i)
        theInfo = info
