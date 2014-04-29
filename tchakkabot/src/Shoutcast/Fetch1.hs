module Shoutcast.Fetch1 (getPage') where

import Shoutcast.ServerStats

import Data.Array as A
import Data.Maybe
import Data.Char (isDigit)

import Data.List (isPrefixOf)

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Regex.Posix

getPage' :: String -> Maybe ServerStats
getPage' = getInfo . parseTags

getInfo :: [Tag String] -> Maybe ServerStats
getInfo tags = let
                 infoTable = head . inside "table" .
                             dropWhile (not . tagText (=="Current Stream Information")) $ tags
                 rows = inside "tr" infoTable
                 cells = map (map innerText . toCells) rows
               in processStuff . map (\[a,b] -> (a,b)) . filter (\r -> length r == 2) $ cells

processStuff :: [(String, String)] -> Maybe ServerStats
processStuff table | "Server is currently down." `isPrefixOf` entry "Server Status: " = Nothing
                   | otherwise = Just $ Stats
                          { maxListeners = maxL
                          , curListeners = curL
                          , uniqListeners = uniqL
                          , peakListeners = read $ entry "Listener Peak: "
                          , strTitle = entry "Stream Title: "
                          , strGenre = entry "Stream Genre: "
                          , strUrl = entry "Stream URL: "
                          , strAIM = entry "Stream AIM: "
                          , strIRC = entry "Stream IRC: "
                          , strICQ = entry "Stream ICQ: "
                          , curSong = entry "Current Song: "
                          , bitRate = bitR
                          , contentType = entry "Content Type: "
                          , nextSong = ""
                          , strHits = Nothing
                          , strPath = ""
                          , avgTime = toSecs (entry "Average Listen Time: ")
                          }
  where entry s = fromMaybe "" $ lookup s table
        (bitR, curL, maxL, uniqL) = getStatusInfo (entry "Stream Status: ")

toSecs :: String -> Int
toSecs = sum . map toSecsPart . words
  where toSecsPart part = let (timePart, unitPart) = span isDigit part
                          in read ("0" ++ timePart) * magnitude unitPart
        magnitude "h" = 3600
        magnitude "m" = 60
        magnitude "s" = 1
        magnitude _   = 0

getStatusInfo :: String -> (Int, Int, Int, Int)
getStatusInfo s = let results = map getElem . matchAllText regex $ s
                      [br,cl,ml,ul] = map (read . fst . (results!!)) [0..3]
                  in (br,cl,ml,ul)
  where regex = makeRegex "[1234567890]+" :: Regex
        getElem a = a A.! 0
  
toCells :: [Tag String] -> [[Tag String]]
toCells = inside "td"

inside :: String -> [Tag String] -> [[Tag String]]
inside name = map (init . tail) . partitions (tagOpenLit name (const True))
