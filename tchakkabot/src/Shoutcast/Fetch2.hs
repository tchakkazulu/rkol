{-# LANGUAGE Arrows #-}

module Shoutcast.Fetch2 (getPage') where

import Shoutcast.ServerStats

import Control.Monad
import Control.Monad.Trans

import Data.Maybe
import Data.Char

import Text.XML.HXT.Core

import System.IO.Unsafe (unsafePerformIO)

getPage' :: String -> Maybe ServerStats
getPage' str = 
  let result = unsafePerformIO $ runX (parseXML str >>> getInfo)
  in case result of
      []    -> Nothing
      (x:_) -> x

parseXML = readString [ withValidate no
                      , withRemoveWS yes
                      ]

atTag, hasNameCI :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasNameCI tag)
hasNameCI n = (getName >>> isA (\x -> map toLower x == map toLower n)) `guards` this

text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText

textAtTag :: ArrowXml a => String -> a XmlTree String
textAtTag tag = (atTag tag >>> text) `orElse` (proc _ -> returnA -< "")

getInfo :: ArrowXml a => a XmlTree (Maybe ServerStats)
getInfo = atTag "SHOUTCASTSERVER" >>>
  proc x -> do
    streamUp <- textAtTag "streamstatus" -< x
    curl  <- textAtTag "currentlisteners" -< x
    maxl  <- textAtTag "maxlisteners" -< x
    uniq  <- textAtTag "uniquelisteners" -< x
    peak  <- textAtTag "peaklisteners" -< x
    title <- textAtTag "servertitle" -< x
    genre <- textAtTag "servergenre" -< x
    url   <- textAtTag "serverurl" -< x
    aim   <- textAtTag "aim" -< x
    icq   <- textAtTag "icq" -< x
    irc   <- textAtTag "irc" -< x
    song  <- textAtTag "songtitle" -< x
    bitr  <- textAtTag "bitrate" -< x
    ctype <- textAtTag "content" -< x
    nexts <- textAtTag "nexttitle" -< x
    hits  <- textAtTag "streamhits" -< x
    path  <- textAtTag "streampath" -< x
    avg   <- textAtTag "averagetime" -< x
    returnA -< if read streamUp == (0 :: Int) then Nothing
                                              else Just $ Stats
      { maxListeners  = read maxl
      , curListeners  = read curl
      , uniqListeners = read uniq
      , peakListeners = read peak
      , strTitle      = title
      , strGenre      = genre
      , strUrl        = url
      , strAIM        = aim
      , strICQ        = icq
      , strIRC        = irc
      , curSong       = song
      , bitRate       = read bitr
      , contentType   = ctype
      , nextSong      = nexts
      , strHits       = Just $ read hits
      , strPath       = path
      , avgTime       = read avg
      }
