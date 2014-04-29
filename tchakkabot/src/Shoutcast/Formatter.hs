module Shoutcast.Formatter where

import Shoutcast.ServerStats
import Formatter

import Data.Char (toLower)
import Data.Maybe (fromMaybe, mapMaybe)

formatterFromFile :: FilePath -> IO ([Maybe ServerStats] -> String)
formatterFromFile fp = do
  fmt' <- parseFormattingFromFile fp
  let dispMe = case fmt' of
        Left _ -> const "Bad formatting. Better build something to replace this."
        Right fmt -> formatStats fmt
  case fmt' of
    Left errs -> print errs
    Right _   -> return ()
  return dispMe

formatStats :: Formatting -> [Maybe ServerStats] -> String
formatStats fmt stats = fromMaybe "Insufficient formatting rules" $ format lookupStats fmt stats (Tag "MAIN")

lookupStats :: Tag -> [Maybe ServerStats] -> Maybe String
lookupStats (Tag tag) stats = case res of
                          [] -> Nothing
                          (x:_) -> Just x
  where res = mapMaybe (>>= lookupStats' (map toLower tag)) stats

lookupStats' :: String -> ServerStats -> Maybe String
lookupStats' "maxlisteners" = numWrap maxListeners
lookupStats' "currentlisteners" = numWrap curListeners
lookupStats' "peaklisteners" = numWrap peakListeners
lookupStats' "uniquelisteners" = numWrap uniqListeners
lookupStats' "servertitle" = strWrap strTitle
lookupStats' "servergenre" = strWrap strGenre
lookupStats' "serverurl" = strWrap strUrl
lookupStats' "aim" = strWrap strAIM
lookupStats' "icq" = strWrap strICQ
lookupStats' "irc" = strWrap strIRC
lookupStats' "songtitle" = strWrap curSong
lookupStats' "bitrate" = numWrap0 bitRate
lookupStats' "content" = strWrap contentType
lookupStats' "nexttitle" = strWrap nextSong
lookupStats' "streamhits" = fmap show . strHits
lookupStats' "streampath" = strWrap strPath
lookupStats' "averagetime" = numWrap avgTime
lookupStats' field = const (Just $ "Unknown field: " ++ show field)

numWrap f = Just . show . f
numWrap0 f stat = case f stat of
                    0 -> Nothing
                    x -> Just $ show x
strWrap f stat = case f stat of
                   "" -> Nothing
                   x  -> Just x
