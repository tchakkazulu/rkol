module Formatter ( parseFormatting
                 , parseFormattingFromFile
                 , format
                 , Formatting
                 , Tag(..)
                 ) where

import Control.Applicative hiding (many)

import Data.Char
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (intercalate)
import Data.Traversable (sequenceA)
import qualified Data.Map as M

import Text.Parsec hiding ((<|>))
import Text.Parsec.String

parseFormatting :: String -> Either ParseError Formatting
parseFormatting = parse (formattingParser <* eof) ""

parseFormattingFromFile :: FilePath -> IO (Either ParseError Formatting)
parseFormattingFromFile = parseFromFile (formattingParser <* eof)

formattingParser :: Parser Formatting
formattingParser = M.fromList <$ spaces <*> many1 (partParser <* spaces)

partParser :: Parser (Tag,[FormatRule])
partParser = (,) . Tag <$> ((many1 alphaNum <* char ':') <?> "tag name") <* newline <*> many1 (ruleParser <* newline)

ruleParser :: Parser FormatRule
ruleParser = many1 atomParser

atomParser :: Parser FormatAtom
atomParser = (Reference . Tag <$ char '{' <*> many1 alphaNum <*> specParser <* char '}' <?> "tag")
         <|> (Literal <$> many1 (noneOf "{}\n") <?> "literal text")

specParser :: Parser Spec
specParser = Specific . read <$ char '@' <*> many1 digit
         <|> All <$ char '*' <*> many (noneOf "{}\n")
         <|> pure Default

type Formatting = M.Map Tag [FormatRule]
type FormatRule = [FormatAtom]
data FormatAtom = Literal String | Reference Tag Spec
  deriving (Eq, Ord, Show)
data Spec = Default | Specific Int | All String
  deriving (Eq, Ord, Show)
newtype Tag = Tag {unTag :: String}
  deriving (Eq, Ord, Show)

format :: (Tag -> [a] -> Maybe String) -> Formatting -> [a] -> Tag -> Maybe String
format myLookup fmt stats = appTag
  where appTag tag = case M.lookup tag fmt of
                   Nothing -> myLookup tag stats
                   Just rules -> foldr (<|>) Nothing (map appRule rules)
        appRule atoms = concat <$> mapM appAtom atoms
        appAtom (Literal s) = pure s
        appAtom (Reference tag Default) = appTag tag
        appAtom (Reference tag (Specific i)) = case drop i stats of
                                                 []    -> Nothing
                                                 (x:_) -> format myLookup fmt [x] tag
        appAtom (Reference tag (All str)) = case res of
                                              [] -> Nothing
                                              _  -> Just $ intercalate str res
          where res = mapMaybe (\stat -> format myLookup fmt [stat] tag) stats
