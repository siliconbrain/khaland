{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Khaland.Game (
    createNewGame,
    loadSavedGame,
    playGame,
) where

--import Character
import Control.Monad.State
import Data.Char (toLower)
import qualified Distance as Dst
import qualified Speed as Spd
import qualified Time as Tm
import Savefile
import System.Console.ANSI
import System.IO
import System.Random

import Khaland.Game.State
import Khaland.Interactive

createNewGame :: IO GameState

playGame :: GameState -> IO ()

processInputWith :: (String -> IO b) -> IO b
processInputWith processor = do
    input <- getLine
    processor $ map toLower input

--data GameState = GameState {
--                     player :: Player,
--                     currentTime :: Tm.Time Float
--                 }

startNewGame :: IO ()
startNewGame = withNewGame (\ file -> (initGame file) >>= continueGame)

initGame :: Handle -> IO Handle
initGame file = do
    stdGen <- newStdGen             -- create new random generator
    hPutStrLn file $ show stdGen    -- serialize it to the file
    hSeek file AbsoluteSeek 0       -- rewind the file handle
    return file

continueGame :: Handle -> IO ()
continueGame file = do
    stdGenStr <- hGetLine file
--    let stdGen = read stdGenStr
    return ()

--    clearScreen
--    putStrLn "Almost three weeks have passed since The Crash.\nLorem ipsum, here comes the back-story, which I'm rather bad at telling."
--    getLine
--    clearScreen
--    putStrLn "You wake up on the floor of what seams like a hut with a throbbing headache. Looking at the ceiling you recognize your home instantly from the familiar ornaments."
--    putStrLn "You're still dizzy while you try to remember how you got on the floor. You close your eyes and concentrate. You see five man. Two of them are standing next to you. " -- continue story here
--    putStrLn "she's my [wife]"
--    putStrLn "he's my [husband]"
--    gender <- processInputWith selectGender
--    putStrLn $ "You're a " ++ (if gender == Male then "tribesman" else "tribeswoman") ++ "."
--    where selectGender input = case input of
--            "wife" -> return Male
--            "husband" -> return Female
--            _ -> do
--                putStrLn "Invalid choice."
--                processInputWith selectGender

--travel dist = do
--    gameState <- get
--    let playerChar = player gameState
--    put $ GameState playerChar ((currentTime gameState) `Tm.offset` (Spd.getTime dist (travelSpeed playerChar)))


