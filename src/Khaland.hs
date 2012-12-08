import Control.Concurrent (threadDelay)
import Control.Monad (foldM_)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Game
import Paths_khaland    -- this is created by Cabal
import Savefile
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
    if null savedGames
        then do
            clearScreen
            putStrLn "You have no saved games yet. Would you like to start a new game?"
            input <- getLine
            case map toLower input of
                "yes" -> startNewGame
                _ -> showMenuScreen
        else selectSavedGame savedGames
    where selectSavedGame savedGames = do
            clearScreen
            putStrLn "Your saved games are:"
            putStrLn "========================"
            foldM_ (\ _ savedGame -> putStrLn savedGame) () savedGames
            putStrLn "========================"
            putStrLn "Please select a saved game to load!"
            processInput savedGames
          processInput savedGames = do
            input <- getLine
            case input of
                [] -> do
                    putStrLn "You didn't select anything. Would you like to start a new game?"
                    input' <- getLine
                    case map toLower input' of
                        "yes" -> startNewGame
                        "quit" -> return ()
                        _ -> selectSavedGame savedGames
                "quit" -> return ()
                otherwise -> do
                    case [ savedGame | savedGame <- savedGames, isPrefixOf input savedGame] of
                        [] -> do
                            putStrLn "You've probably misspelled something. Please try again!"
                            processInput savedGames
                        [selectedGame] -> withSavedGame selectedGame continueGame
                        ambiguousSelections -> do
                            putStrLn "You weren't specific enough. Which would you like to continue?"
                            processInput savedGames

showMenuScreen :: IO ()
showMenuScreen = do
    clearScreen
    -- could show some artwork here
    putStrLn "Start new game [start]"
    putStrLn "Load existing game [load]"
    putStrLn "Quit [quit]"
    processInput
    where processInput = do
            input <- getLine
            case map toLower input of
                [] -> do
                    putStrLn "Silence is not the answer. Please tell me what you'd like to do!"
                    processInput
                "start" -> startNewGame
                "load" -> showLoadGameScreen
                "quit" -> return ()
                _ -> do
                    putStrLn "I don't know how to do that. Try to select from the options above!"
                    processInput

main :: IO ()
main = do
    playIntro
    showMenuScreen
    putStrLn "\n========\nIn memoriam Szalontai Andor\n========\n\nThanks for playing!"
