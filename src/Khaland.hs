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

showIntroScreen :: [String] -> IO [String]
showIntroScreen [] = return []
showIntroScreen (x:xs)
    | null x = return xs
    | otherwise = do
        putStrLn x
        showIntroScreen xs

playIntroScreens :: [String] -> IO ()
playIntroScreens [] = return ()
playIntroScreens (x:xs)
    | null x = do
        clearScreen
        xs' <- showIntroScreen xs
        -- might have to put a "Press ENTER to continue" text just to clarify the situation
        getLine
        playIntroScreens xs'
    | otherwise = do
        clearScreen
        xs' <- showIntroScreen xs
        threadDelay $ read x
        playIntroScreens xs'

playIntro :: IO ()
playIntro = do
    introData <- readDataFile "intro.txt"
    playIntroScreens $ lines introData

showMenuScreen :: IO ()
showMenuScreen = do
    clearScreen
    putStrLn "(s) Start new game\n(l) Load existing game\n(q) Quit"
    input <- getChar
    case input of
        's' -> do
            clearScreen
            putStrLn "Starting new game..."
        'l' -> do
            clearScreen
            putStrLn "Please select a game to load!"
            gameNumber <- getLine
            putStrLn $ "Loading game no. " ++ gameNumber ++ "..."
        'q' -> clearScreen
        _ -> do
            putStrLn "Invalid input"
            getLine
            showMenuScreen

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    playIntro
    showMenuScreen
    putStrLn "Thanks for playing!"
