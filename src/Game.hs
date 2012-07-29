module Game (
    GameState,
    getSavedGames,
    startGame,
    loadGame,
) where

import Character
import Control.Monad.State
import qualified Distance as Dst
import qualified Speed as Spd
import qualified Time as Tm

data GameState = GameState {
                     player :: Player,
                     currentTime :: Tm.Time Float
                 }

getSavedGames :: IO [(String,String,String)]
getSavedGames = do
    -- do directory search here
    return []

startGame :: IO ()
startGame = putStrLn "Starting new game..."

loadGame :: String -> IO ()
loadGame file = putStrLn $ "Loading game from " ++ file ++ "..."

travel dist = do
    gameState <- get
    let playerChar = player gameState
    put $ GameState playerChar ((currentTime gameState) `Tm.offset` (Spd.getTime dist (travelSpeed playerChar)))


