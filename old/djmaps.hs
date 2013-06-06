module Main where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

import Network.HTTP
import Network.Browser
import Network.URI

import Data.Maybe

main :: IO ()
main = do
  page <- browse $ getPage "http://radio-kol.net/wiki/index.php?title=DJ_Mapping_Info"
  writeFile "djmaps.txt" (concat . filter (not . null) . getInfo . parseTags $ page)



getPage :: String -> BrowserAction (HandleStream String) String
getPage url = do
--  setOutHandler (\_ -> return ())
  (_, resp) <- request (req url)
  return (rspBody resp)

req :: String -> Request String
req url = Request { rqURI = fromJust $ parseURI url
                  , rqMethod = GET
                  , rqHeaders = []
                  , rqBody = ""
                  }


getInfo :: [Tag String] -> [String]
getInfo = map innerText . getEnum . takeWhile (not . tagText (=="*END MAPS*")) . dropWhile (not . tagText (=="*BEGIN MAPS*"))

getEnum :: [Tag String] -> [[Tag String]]
getEnum = partitions (tagOpenLit "li" (const True))

