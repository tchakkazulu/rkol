module IRCBot.Whois ( WhoisRecord(..)
                    , ModeChar
                    , performWhois
                    ) where

import IRCBot

data WhoisRecord = WR { nick :: String
                      , uname :: String
                      , host :: String
                      , rname :: String
                      , server :: String
                      , sinfo :: String
                      , ircop :: Bool
                      , idle :: Int
                      , away :: Maybe String
                      , chans :: [(String, ModeChar)]
                      }
  deriving (Show)

emptyWR :: WhoisRecord
emptyWR = WR "" "" "" "" "" "" False 0 Nothing []

type ModeChar = Char

performWhois :: String -> (WhoisRecord -> Bot s ()) -> Bot s ()
performWhois nick = performCall (emptyWR{nick = nick})
                                (Message Nothing "WHOIS" [nick])
                                (map (\num -> (ENum num, whoisEv)) [301,311,312,313,317,319])
                                (ENum 318)
                                
whoisEv :: Message -> WhoisRecord -> WhoisRecord
whoisEv (Message _ "301" [_,_,amsg]) wr = wr{away = Just amsg}
whoisEv (Message _ "311" [_,_,usr,hst,_,rn]) wr = wr{uname = usr, host = hst, rname = rn}
whoisEv (Message _ "312" [_,_,s,si]) wr = wr{server = s, sinfo = si}
whoisEv (Message _ "313" _) wr = wr{ircop = True}
whoisEv (Message _ "317" [_,_,i,_]) wr = wr{idle = read i}
whoisEv (Message _ "319" [_,_,chs]) wr = wr{chans = mychans' ++ chans wr}
  where mychans' = map toThing . words $ chs
        toThing ch@('#':_) = (ch,' ')
        toThing (c:ch) = (ch,c)
whoisEv _ wr = wr
