module Khaland.Game.Persistence (
    getSavedGames,
    withNewGame,
    withSavedGame,
) where

import Control.Monad (filterM)
import Data.List
import Data.Time (getCurrentTime)
import System.Directory
import System.FilePath
import System.IO

import Khaland.Game.State

data SavedGame = SavedGame UTCTime FilePath

getSaveFileDirectory :: IO FilePath
getSaveFileDirectory = do
    appDataDir <- getAppUserDataDirectory "khaland"
    let saveFileDir = appDataDir </> "saved-games"
    createDirectoryIfMissing True saveFileDir
    return saveFileDir

getFilesInDirectory :: FilePath -> IO [FilePath]
getFilesInDirectory dir = do
    contents <- getDirectoryContents dir
    filterM doesFileExist $ map (dir </>) contents

getSaveFiles :: IO [FilePath]
getSaveFiles = getSaveFileDirectory >>= getFilesInDirectory 

getSavedGames :: IO [SavedGame]
getSavedGames = getSaveFiles >>= mapM getSavedGameInfo
    where
        getSavedGameInfo file = do
            info <- withFile file ReadMode readSavedGameInfo
            return $ SavedGame info file
        readSavedGameInfo handle = do
            firstLine <- hGetLine handle
            return $ read firstLine

loadSavedGame :: SavedGame -> IO GameState
loadSavedGame (SavedGame _ file) = loadGameFromFile file

loadGameFromFile :: FilePath -> IO GameState


saveGameToFile :: GameState -> FilePath -> IO ()

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
