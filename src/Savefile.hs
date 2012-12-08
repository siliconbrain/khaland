module Savefile (
    getSavedGames,
    withNewGame,
    withSavedGame,
) where

import Control.Monad(filterM)
import Data.List
import Data.Time (getCurrentTime)
import System.Directory
import System.IO

getSavedGamesDirectory :: IO FilePath
getSavedGamesDirectory = do
    appDataDir <- getAppUserDataDirectory "khaland"
    let savedGamesDir = appDataDir ++ "/saved-games"
    createDirectoryIfMissing True savedGamesDir
    return $ savedGamesDir

getSavedGames :: IO [FilePath]
getSavedGames = do
    savedGamesDir <- getSavedGamesDirectory
    contents <- getDirectoryContents savedGamesDir
    filterM (\ content -> (getSavedGameFilePath content) >>= doesFileExist) contents

getSavedGameFilePath :: String -> IO FilePath
getSavedGameFilePath savedGame = do
    savedGamesDir <- getSavedGamesDirectory
    return $ savedGamesDir ++ "/" ++ savedGame

withSavedGame :: String -> (Handle -> IO ()) -> IO ()
withSavedGame savedGame f = do
    filePath <- getSavedGameFilePath savedGame
    withFile filePath ReadWriteMode f

withNewGame :: (Handle -> IO ()) -> IO ()
withNewGame f = do
    fileName <- getFileName
    withSavedGame fileName f
    where getFileName = do
            time <- getCurrentTime
            return $ "save-" ++ (cleanTimeString time)
          cleanTimeString time = tillSecs [ replaceOffending c | c <- show time ]
          replaceOffending c = case c of
            ' ' -> '-'
            ':' -> '-'
            _ -> c
          tillSecs s = takeWhile (/= '.') s
