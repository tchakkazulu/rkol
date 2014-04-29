{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Config where

import qualified Data.ByteString.Lazy as BS

import Data.Char (toLower)

import qualified Data.Map as M
import Data.Map (Map)

import Shoutcast.Fetch (ServerURL(..))

import Data.Aeson
import Data.Aeson.TH

data Config = Config { cfgirc :: IRCDetails
                     , cfgkol :: KoLDetails
                     , cfgradio :: Radio
                     }
  deriving (Show)

data IRCDetails = IRC { ircserver :: String
                      , ircport :: Int
                      , ircnick :: String
                      , ircuser :: String
                      , ircreal :: String
                      , ircchannels :: [String]
                      }
  deriving (Show)

data KoLDetails = KoL { kollogin :: Maybe String
                      , kolpass :: Maybe String
                      , kolchannels :: [String]
                      }
  deriving (Show)

data Radio = Radio { rurls :: [ServerURL]
                   , rformat :: FilePath
                   }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Config)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''IRCDetails)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''KoLDetails)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Radio)

instance FromJSON ServerURL where
  parseJSON (Object v) = do
    x <- v .:? "type" .!= ""
    let c = case map toLower x of
              "v1" -> V1
              "v2" -> V2XML
              "v2xml" -> V2XML
              "v2html" -> V2HTML
              _ -> Unknown
    url <- v .: "url"
    return $ c url

-- Let's help type inference, what with OverloadedStrings enabled and all.
str :: String -> String
str = id

instance ToJSON ServerURL where
  toJSON (V1 url) = object ["type" .= str "v1", "url" .= url]
  toJSON (V2XML url) = object ["type" .= str "v2xml", "url" .= url]
  toJSON (V2HTML url) = object ["type" .= str "v2html", "url" .= url]
  toJSON (Unknown url) = object ["url" .= url]

configFromFile :: FilePath -> IO Config
configFromFile fp = do
  cfg' <- BS.readFile fp
  case decode cfg' of
    Nothing -> error $ "Bad config file: " ++ fp
    Just cfg -> return cfg

  