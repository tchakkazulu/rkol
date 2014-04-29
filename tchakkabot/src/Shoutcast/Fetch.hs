module Shoutcast.Fetch (getPage, getPageWith, ServerURL(V1,V2XML,V2HTML,Unknown)) where

import Control.Exception
import Prelude hiding (catch)

import Data.Maybe

import Network
import Network.Browser
import Network.HTTP
import Network.URI

import qualified Shoutcast.Fetch1 as F1
import qualified Shoutcast.Fetch2 as F2
import Shoutcast.ServerStats

data ServerURL = V1 {server :: String}
               | V2XML {server :: String}
               | V2HTML {server :: String}
               | Unknown {server :: String}
  deriving (Eq, Ord, Show)

getPage :: ServerURL -> IO (Maybe ServerStats)
getPage url = getPageWith (appropriateReader url) (server url)

errH = flip catch (\(SomeException e) -> return Nothing)

appropriateReader (V1 _) = F1.getPage'
appropriateReader (V2XML _) = F2.getPage'
appropriateReader (V2HTML _) = F1.getPage'
appropriateReader (Unknown _) = unknownGetPage

getPageWith :: (String -> Maybe ServerStats) -> String -> IO (Maybe ServerStats)
getPageWith f url = errH . browse $ do
  setOutHandler (\_ -> return ())
  (_,resp) <- request (req url)
  return . f $ rspBody resp

unknownGetPage :: String -> Maybe ServerStats
unknownGetPage str@('<':'?':_) = F2.getPage' str
unknownGetPage str@('<':'h':_) = F1.getPage' str
unknownGetPage str = Nothing

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
