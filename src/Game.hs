{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Game (
    GameState,
    startNewGame,
    continueGame,
) where

import Character
import Control.Monad.State
import Data.Char (toLower)
import qualified Distance as Dst
import qualified Speed as Spd
import qualified Time as Tm
import System.Console.ANSI
import System.IO

processInputWith :: (String -> IO b) -> IO b
processInputWith processor = do
    input <- getLine
    processor $ map toLower input

data GameState = GameState {
                     player :: Player,
                     currentTime :: Tm.Time Float
                 }

startNewGame :: IO ()
startNewGame = do
    clearScreen
    putStrLn "Almost three weeks have passed since The Crash.\nLorem ipsum, here comes the back-story, which I'm rather bad at telling."
    getLine
    clearScreen
    putStrLn "You wake up on the floor of what seams like a hut with a throbbing headache. Looking at the ceiling you recognize your home instantly from the familiar ornaments."
    putStrLn "You're still dizzy while you try to remember how you got on the floor. You close your eyes and concentrate. You see five man. Two of them are standing next to you. " -- continue story here
    putStrLn "she's my [wife]"
    putStrLn "he's my [husband]"
    gender <- processInputWith selectGender
    putStrLn $ "You're a " ++ (if gender == Male then "tribesman" else "tribeswoman") ++ "."
    where selectGender input = case input of
            "wife" -> return Male
            "husband" -> return Female
            _ -> do
                putStrLn "Invalid choice."
                processInputWith selectGender

continueGame :: Handle -> IO ()
continueGame file = return ()

travel dist = do
    gameState <- get
    let playerChar = player gameState
    put $ GameState playerChar ((currentTime gameState) `Tm.offset` (Spd.getTime dist (travelSpeed playerChar)))


