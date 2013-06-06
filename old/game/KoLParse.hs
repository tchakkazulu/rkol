module KoLParse where

import Text.HTML.TagSoup hiding (Tag)
import qualified Text.HTML.TagSoup as Soup
import Text.HTML.TagSoup.Match
-- import Text.HTML.TagSoup.Render

import ParseLib

import KoLChat

type Tag = Soup.Tag String

parse :: String -> Maybe ([KoLMsg], String)
parse = runParser ((,) <$> messages <*> nextOne <* input) . parseTags

messages :: Parser Tag [KoLMsg]
messages = many message

nextOne :: Parser Tag String
nextOne = (drop 9 . deComment) <$> satisfy (tagComment (const True))
  where deComment (TagComment s) = s

message :: Parser Tag KoLMsg
message = (chanMsg <|> privMsg <|> otherMsg) <* tagO "br"

chanMsg = chanMessage <$ tagO "font" <*> tagT <* tagC "font" <* tagT <* tagO "b" <* tagO "a" <* tagO "font" <*> tagT <* tagC "font" <* tagC "b" <* tagC "a" <*> tagT
privMsg = privMessage <$ tagO "a" <* tagO "font" <* tagO "b" <*> tagT <* tagC "b" <* tagC "a" <*> tagT <* tagC "font"

chanMessage chan from msg = ChanMessage (init.tail $ chan) from (drop 2 msg)
privMessage from msg = PrivMessage from' (tail msg)
  where from' = init . takeWhile (/= '(') $ from

tagO, tagC :: String -> Parser Tag Tag
tagT :: Parser Tag String
tagO = satisfy . tagOpenNameLit
tagC = satisfy . tagCloseNameLit
tagT = fromTagText <$> satisfy isTagText

otherMsg :: Parser Tag KoLMsg
otherMsg = Other . renderTags <$> many (satisfy (not . tagOpenNameLit "br"))


input :: Parser s [s]
input s = [(s,[])]

