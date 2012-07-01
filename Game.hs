module Game (
    loadGame,
    processInput,
    putScreen,
    Screen,
) where

import Control.Monad
import Data.List
import System.Console.ANSI

data Action = Action { getKey :: Char, getDescription :: String, getNextScreen :: Maybe Screen }

data Screen = Screen { getText :: String, getActions :: [Action] }

type InputError = String

processInput :: String -> Screen -> Either InputError (Maybe Screen)
processInput [] screen = Left "Please choose an action to perform!"
processInput (c:_) screen = case find (\x -> getKey x == c) (getActions screen) of
    Nothing -> Left "Not a valid choice."
    Just action -> Right (getNextScreen action)

putScreen :: Screen -> IO ()
putScreen screen = do
    clearScreen
    putStrLn $ getText screen
    putStrLn "\nWhat would you like to do?"
    foldM (\acc action -> putStrLn $ "(" ++ getKey action : ") " ++ getDescription action) () (getActions screen)

loadGame :: IO Screen
loadGame = do
    asciiLogo <- readFile "res/logo.txt"
    return $ Screen (asciiLogo ++ "\n\ta text-based adventure game by siliconbrain") [Action 'q' "Quit" Nothing]
