import Control.Concurrent (threadDelay)
import Control.Monad.Cont
import Control.Monad.State
import Game
import Paths_khaland    -- this is created by Cabal
import System.Console.ANSI
import System.IO

readDataFile :: String -> IO String
readDataFile relativeFileName = do
    absoluteFileName <- getDataFileName relativeFileName
    readFile absoluteFileName

type PreGameAction = IO ()

load :: IO PreGameAction
load = do
    khalandLogo <- readDataFile "logo.txt"
    return $ do
        clearScreen
        putStrLn $ khalandLogo ++ "\n\ta text-based adventure game by siliconbrain\n\nPress ENTER to continue"
        getLine
        return ()

main :: IO ()
main = do
    preGameAction <- load
    preGameAction
    --(gameState, cont) <- preGameAction
    --runCont cont (`runState` gameState)
    putStrLn "Thanks for playing!"
