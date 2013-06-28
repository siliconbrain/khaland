module Khaland.Content (
    readContentFile
  ) where

import Paths_khaland (getDataFileName)   -- this package is automatically created by Cabal

readContentFile :: String -> IO String
readContentFile fileName = do
    absoluteFilePath <- getDataFileName fileName
    readFile absoluteFilePath
