module Fetch where

import Data.Array as A
import Data.Maybe

import Network
import Network.Browser
import Network.HTTP
import Network.URI

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Regex.Posix


getPage :: String -> BrowserAction (HandleStream String) (Maybe ServerStats)
getPage url = do
  setOutHandler (\_ -> return ())
  (_,resp) <- request (req url)
  return (getInfo . parseTags . rspBody $ resp)

getInfo :: [Tag String] -> Maybe ServerStats
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
  
toCells :: [Tag String] -> [[Tag String]]
toCells = inside "td"

inside :: String -> [Tag String] -> [[Tag String]]
inside name = map (init . tail) . partitions (tagOpenLit name (const True))


data ServerStats = Stats { maxListeners :: Int
                         , curListeners :: Int
                         , strTitle :: String
                         , strGenre :: String
                         , strUrl :: String
                         , strAIM :: String
                         , strIRC :: String
                         , curSong :: String
                         }

req :: String -> Request String
req url = Request { rqURI = fromJust $ parseURI url
                  , rqMethod = GET
                  , rqHeaders = [ Header HdrHost url
                                , Header HdrUserAgent "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.6) Gecko/2009020409 Iceweasel/3.0.6 (Debian-3.0.6-1)"
                                , Header HdrAccept "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
                                , Header HdrConnection "keep-alive"
                                ]
                  , rqBody = ""
                  }

