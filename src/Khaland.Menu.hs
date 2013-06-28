module Khaland.Menu (
    showMainMenu,
    MenuCommand(..),
  ) where

import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.Console.ANSI
import System.IO

import Khaland.Game
import Khaland.Game.Persistence

data MenuCommand
  = PlayGame GameState
  | ExitGame
  deriving (Eq)

data MenuItem = MenuItem String String (IO MenuCommand)

mainMenuItems =
    [ MenuItem "Start new game" "new" startNewGame
    , MenuItem "Load existing game" "load" showLoadGameMenu
    , MenuItem "Quit" "quit" (return ExitGame)
    ]
    where
        startNewGame = do
            gameState <- createNewGame
            return PlayGame gameState

showMainMenu :: IO MenuCommand
showMainMenu = do
    clearScreen
    -- could show some artwork here
    mapM_ printMenuItem mainMenuItems
    putStrLn "Please select one of the options above! (Type in the text inside the brackets next to the option and press Enter!)"
    processInput
    where
        printMenuItem (MenuItem text cmd) = do
            putStr text
            putStrLn $ " [" ++ cmd ++ "]"
        processInput = do
            input <- getLine
            case map toLower input of
                [] -> do
                    putStrLn "Silence is not the answer. Please tell me what you'd like to do!"
                    processInput
                cmd -> case find (\ MenuItem _ c _ -> c == cmd) mainMenuItems of
                    Nothing -> do
                        putStrLn "I don't know how to do that. Try to select from the options above!"
                        processInput
                    Just (MenuItem _ _ action) -> action


showLoadGameMenu :: IO MenuCommand
showLoadGameMenu = do
    savedGames <- getSavedGames
    if null savedGames
        then do
            clearScreen
            putStrLn "You have no saved games yet. Would you like to start a new game?"
            input <- getLine
            case map toLower input of
                "yes" -> startNewGame
                _ -> showMainMenu
        else selectSavedGame savedGames
    where selectSavedGame savedGames = do
            clearScreen
            putStrLn "Your saved games are:"
            putStrLn "========================"
            mapM_ putStrLn savedGames
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
                        "quit" -> return ExitGame
                        _ -> selectSavedGame savedGames
                "quit" -> return ExitGame
                otherwise -> do
                    case [savedGame | savedGame <- savedGames, isPrefixOf input savedGame] of
                        [] -> do
                            putStrLn "You've probably misspelled something. Please try again!"
                            processInput savedGames
                        [selectedGame] -> withSavedGame selectedGame continueGame
                        ambiguousSelections -> do
                            putStrLn "You weren't specific enough. Which would you like to continue?"
                            processInput savedGames
