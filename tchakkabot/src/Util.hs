module Util where

import Paths_tchatbot

import System.Directory

getDataFileName' :: FilePath -> IO FilePath
getDataFileName' fp = do
  p <- doesFileExist fp
  if p then return fp
       else getDataFileName fp

  