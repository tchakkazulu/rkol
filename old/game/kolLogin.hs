import Network.HTTP
import Network.Browser
import Network.URI

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

import Control.Exception

import Control.Concurrent
import Control.Concurrent.Chan

import Data.Char

import Text.HTML.TagSoup hiding (Tag)
import Text.HTML.TagSoup.Match

import Text.Regex.Posix
import qualified Data.Array as A

import Data.Maybe

import ParseLib

import Prelude hiding (catch)

import KoLChat
import KoLParse as P

main = do

  theTube <- newChan
  tid <- forkIO (listener theTube)

  [nick,pass] <- (readFile "kolConf" >>= return . words)
  uri <- browse $ do
    setOutHandler (\_ -> return())
    kolPage@(kolURI, _) <- request . uri2req . fromJust . parseURI $ "http://www.kingdomofloathing.com"
    request . formToRequest $ kolForm kolPage nick pass

    -- We are logged in now :)
    (_,rsp) <- request . uri2req . fromJust . parseURI $ basename kolURI ++ "lchat.php"
    let (uid, pwd) = getuidpwd (rspBody rsp)
    ioAction $ print (uid, pwd)

    -- Let's chat!
    flip runReaderT (basename kolURI, uid, pwd) $ chatScript theTube

    return kolURI
  writeFile "meh.txt" (basename uri)

kolForm (kolURI, kolResponse) nick pass = Form POST kolURI $
  [("loggingin","Yup.")
  ,("loginname", nick)
  ,("password", pass)
  ,("secure", "0")
  ,("challenge", getChallenge (rspBody kolResponse))
  ,("response", "")
  ,("submitbutton", "Log In")
  ]

type Environment = (String, String, String)

type ChatScript a = ReaderT Environment (BrowserAction (HandleStream [Char])) a



-- The chat script

chatScript :: Chan String -> ChatScript ()
chatScript tube = do
  chat' "/idleoff"
--  chat' "/c hobopolis"
--  chat' "/l clan"
  chat' "/l radio"
--  chat $ ChanMessage "clan" "Tchakka" "This is here so I know stuff works."
--  chat "Now testing chatbot code!"
  let bot num = do
        p <- doIO $ isEmptyChan tube
        if p then return ()
             else do
               line <- doIO $ readChan tube
               doIO $ putStrLn "We got songs to announce!"
               doIO $ appendFile "songlog" line
               doIO $ appendFile "songlog" "\n"
               chat $ ChanMessage "radio" "Tchakka" line
               return ()
        (msgs,newnum) <- getNewPage num
        mapM_ handleMsg msgs
        doIO $ putStrLn "Messages:"
        doIO $ print msgs
        wait 15
        bot newnum
  bot "0"


handleMsg :: KoLMsg -> ChatScript ()
handleMsg (PrivMessage from "!quit") | map toLower from `elem` allowedShutDown = shutDown
handleMsg (ChanMessage "radio" "chatbot" msg) = shutDown
handleMsg _                                   = return ()

allowedShutDown = ["tchakka","tchakkazulu","amilianna","ailurodragon","pasta far eye", "kolmohdee","saskia","opai"]

shutDown = do (base,uid,pwd) <- ask
              lift . request . uri2req . fromJust . parseURI $ base ++ "logout.php"
              return ()

doIO = lift . ioAction

getNewPage :: String -> ChatScript ([KoLMsg], String)
getNewPage num = do
  (base, uid, pwd) <- ask
--  doIO $ writeFile "moo.txt" base
  (_,rsp) <- lift . request . uri2req . fromJust . parseURI $ base ++ "newchatmessages.php?lasttime=" ++ num  
  doIO $ do
    putStrLn (rspBody rsp)
    appendFile "moo.txt" "START:\n"
    appendFile "moo.txt" (rspBody rsp)
    appendFile "moo.txt" "STOP:\n"
  case P.parse (rspBody rsp) of
    Just x -> return x
    Nothing -> return ([Other (show $ parseTags (rspBody rsp))],nextPage (rspBody rsp))

nextPage :: String -> String
nextPage s = case (matchAllText regex s) of
                 []      -> error "No last!"
                 (res:_) -> fst (res A.! 1)
  where regex :: Regex
        regex = makeRegex "lastseen:([^-]+)"

-- Chat library

wait :: Int -> ChatScript ()
wait sec = doIO $ threadDelay (sec * 1000000)

chat :: KoLMsg -> ChatScript (URI, Response [Char])
chat = chat' . toStr

chat' :: String -> ChatScript (URI, Response [Char])
chat' line = do
  (base, uid, pwd) <- ask
  lift $ do
    request . uri2req . fromMaybe (error line) . parseURI $ (chatURI base uid pwd ++ urlEncode line)



-- Connection helpers

getChallenge s = case (matchAllText regex s) of
                   []      -> error "No challenge"
                   (res:_) -> fst (res A.! 1)
  where regex :: Regex
        regex = makeRegex "name=challenge value=\"([^\"]*)\""

uri2req uri = Request{ rqURI = uri,
                       rqMethod = GET,
                       rqHeaders = [],
                       rqBody = "" }

requestSite = request . uri2req . fromJust . parseURI

basename :: URI -> String
basename uri = uriScheme uri ++ "//" ++ (uriRegName . fromJust . uriAuthority $ uri) ++ "/"

chatURI base uid pwd = base ++ "submitnewchat.php?playerid=" ++ uid ++"&pwd=" ++ pwd ++ "&graf="

getuidpwd s = case (matchAllText regex s) of
                []       -> error "No uid and pwd"
                (info:_) -> (fst $ info A.! 1, fst $ info A.! 2)
  where regex :: Regex
        regex = makeRegex "playerid=([^&]*)&pwd=([^&]*)"



-- chatbot code

listener :: Chan String -> IO ()
listener theTube = 
  flip evalStateT emptyState $ do
  fetchDJMappings
  forever $ do
    lastSng <- gets lastSong
    liftIO $ print "Letsagooooo!"
    server24 <- liftIO $ errH $ browse $ getPage "http://209.9.238.5" 8792
    server64 <- liftIO $ errH $ browse $ getPage "http://209.9.238.5" 8794
    modify $ \st -> st{srv24 = server24, srv64 = server64}
    let theInfo = msum [server64, server24]
    let now = maybe "Error reading from servers." curSong theInfo
    liftIO $ putStr $ lastSng ++ " -> " ++ now
    when (now /= lastSng && not (now =~ "rkol")) $ do
      displayInfo theTube [server24,server64]
    modify $ \st -> st{lastSong = now}
    liftIO $ threadDelay (20 * 1000000)

errH = flip catch (\(SomeException e) -> return Nothing)


type ChatBot a = StateT MyState IO a

displayInfo :: Chan String -> [Maybe ServerStats] -> ChatBot ()
displayInfo tube [info24, info64] = do
    filters <- gets djFilter
    let res = rest filters
    liftIO $ print $ res
    liftIO $ writeChan tube res
  where rest filters = case theInfo of
          Nothing -> "Could not read server information."
          Just _  -> "Radio KoL: " ++ curS ++ " - " ++ which filters ++ " - " ++ strG ++ " -" ++ info24' ++ info64' {- ++ info128' -}
        strG = maybe "" strGenre theInfo
        curS = maybe "" curSong theInfo
        info24' = maybe "" (\i -> " 24kbps: " ++ info i) info24
        info64' = maybe "" (\i -> " 64kbps: " ++ info i) info64
--        info128' = maybe "" (\i -> " 128kbps: " ++ info i) info128
        info i = show (curListeners i) ++ "/" ++ show (maxListeners i)
        which filters = fromMaybe strG . filters $ fromJust theInfo
        theInfo = info64 `mplus` info24



getPage :: String -> Int -> BrowserAction (HandleStream String) (Maybe ServerStats)
getPage url prt = do
  setOutHandler (\_ -> return ())
  (_,resp) <- request (req url prt)
  return (getInfo . parseTags . rspBody $ resp)

getInfo :: [Tag] -> Maybe ServerStats
getInfo tags = let
                 infoTable = head . inside "table" .
                             dropWhile (not . tagText (=="Current Stream Information")) $ tags
                 rows = inside "tr" infoTable
                 cells = map (map innerText . toCells) rows
               in processStuff . map (\[a,b] -> (a,b)) . filter (\r -> length r == 2) $ cells

processStuff :: [(String, String)] -> Maybe ServerStats
processStuff table = case entry "Server Status: " of
                       "Server is currently down." -> Nothing
                       _                           -> Just $ Stats maxL curL strT strG strU strA strI curS
  where entry s = fromMaybe "" $ lookup s table
        strT = entry "Stream Title: "
        strG = entry "Stream Genre: "
        strU = entry "Stream URL: "
        strA = entry "Stream AIM: "
        strI = entry "Stream IRC: "
        curS = entry "Current Song: "
        (curL, maxL) = getListeners (entry "Stream Status: ")

getListeners :: String -> (Int, Int)
getListeners s = let results = map getElem . matchAllText regex $ s
                 in (read $ fst (results !! 1), read $ fst (results !! 2))
  where regex = makeRegex "[1234567890]+" :: Regex
        getElem a = a A.! 0
  
toCells :: [Tag] -> [[Tag]]
toCells = inside "td"

inside :: String -> [Tag] -> [[Tag]]
inside name = map (init . tail) . partitions (tagOpenLit name (const True))


data MyState = St { srv24 :: Maybe ServerStats
                  , srv64 :: Maybe ServerStats
                  , djFilter :: ServerStats -> Maybe String
                  , lastSong :: String
                  , hasChatbot :: Bool
                  }

data ServerStats = Stats { maxListeners :: Int
                         , curListeners :: Int
                         , strTitle :: String
                         , strGenre :: String
                         , strUrl :: String
                         , strAIM :: String
                         , strIRC :: String
                         , curSong :: String
                         }

emptyState :: MyState
emptyState = St Nothing Nothing {- Nothing -} (const Nothing) "" False

req :: String -> Int -> Request String
req url prt = Request { rqURI = fromJust $ parseURI (url ++ ":" ++ show prt)
                      , rqMethod = GET
                      , rqHeaders = [ Header HdrHost (url ++ ":" ++ show prt)
                                    , Header HdrUserAgent "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.6) Gecko/2009020409 Iceweasel/3.0.6 (Debian-3.0.6-1)"
                                    , Header HdrAccept "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
                                    , Header HdrConnection "keep-alive"
                                    ]
                      , rqBody = ""
                      }


fetchDJMappings :: ChatBot ()
fetchDJMappings = do
  file <- liftIO $ lines `fmap` readFile "djmaps.txt"
  let filters = map getFilter . filter (not . null) $ file
  modify $ \st -> st{djFilter = \stats -> msum (map ($stats) filters)}
  return ()
  

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

