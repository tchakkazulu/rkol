module Shoutcast.ServerStats where

data ServerStats = Stats { maxListeners :: Int
                         , curListeners :: Int
                         , uniqListeners :: Int
                         , peakListeners :: Int
                         , strTitle :: String
                         , strGenre :: String
                         , strUrl :: String
                         , strAIM :: String
                         , strICQ :: String
                         , strIRC :: String
                         , curSong :: String
                         , bitRate :: Int
                         , contentType :: String
                         , nextSong :: String
                         , strHits :: Maybe Int
                         , strPath :: String
                         , avgTime :: Int
                         }
  deriving (Eq, Ord, Show)
