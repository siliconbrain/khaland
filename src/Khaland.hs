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

showLoadGameScreen :: IO ()
showLoadGameScreen = do
    savedGames <- getSavedGames
    clearScreen
    if null savedGames
        then do
            putStrLn "There are no saved games.\nPlease press ENTER to continue!"
            getLine
            showMenuScreen
        else do
            foldM_ (\i (name,date,file) -> (putStrLn $ "(" ++ (show i) ++ ") " ++ name ++ " - " ++ date) >> return (i + 1)) 0 savedGames
            putStrLn "Please select a game to load!"
            processInput savedGames
    where processInput savedGames = do
            input <- getLine
            if null input
                then do
                    putStrLn "No game selected.\nPlease type in one of the options above before pressing ENTER!"
                    processInput savedGames
                else case reads input of
                    [(n,_)] -> if n < 0 || n >= length savedGames
                        then do
                            putStrLn "Invalid option.\nPlease select one of the options from above!"
                            processInput savedGames
                        else let (_,_,file) = savedGames !! n in loadGame file
                    _ -> do
                        putStrLn "Invalid option.\nPlease select one of the options from above!"
                        processInput savedGames

showMenuScreen :: IO ()
showMenuScreen = do
    clearScreen
    -- could show some artwork here
    putStrLn "(s) Start new game"
    putStrLn "(l) Load existing game"
    putStrLn "(q) Quit"
    processInput
    where processInput = do
            input <- getLine
            case input of
                [] -> do
                    putStrLn "No option selected.\nPlease type in one of the options above before pressing ENTER!"
                    processInput
                "s" -> startGame
                "l" -> showLoadGameScreen
                "q" -> return ()
                _ -> do
                    putStrLn "Invalid option selected.\nPlease select one of the options from above!"
                    processInput

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    playIntro
    showMenuScreen
    putStrLn "Thanks for playing!"
