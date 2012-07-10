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

playIntro :: [String] -> IO ()
playIntro [] = return ()
playIntro (x:xs) = do
    clearScreen
    putStrLn x
    threadDelay 1000000
    playIntro xs

putWelcomeScreen :: String -> IO ()
putWelcomeScreen text = do
    clearScreen
    putStrLn text
    getLine
    return ()

load = do
    khalandLogo <- readDataFile "logo.txt"
    playIntro ["\tsiliconbrain\n\t\tpresents"]
    putWelcomeScreen $ khalandLogo ++ "\n\ta text-based adventure game by siliconbrain\n\nPress ENTER to continue"

main :: IO ()
main = do
    load
    putStrLn "Thanks for playing!"
