module Game (
    loadGame,
    processInput,
    putScreen,
    Screen,
) where

import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import System.Console.ANSI

data Action = Action { getDescription :: String, getNextScreen :: Maybe Screen }

data Screen = Screen { getText :: String, getActions :: Map.Map Char Action }

type InputError = String

processInput :: String -> Screen -> Either InputError (Maybe Screen)
processInput [] screen = Left "Please choose an action to perform!"
processInput (c:_) screen = case Map.lookup c (getActions screen) of
    Nothing -> Left "Not a valid choice."
    Just action -> Right (getNextScreen action)

putScreen :: Screen -> IO ()
putScreen screen = do
    clearScreen
    putStrLn $ getText screen
    putStrLn "\nWhat would you like to do?"
    foldM_ (\ _ (key, action) -> putStrLn $ "(" ++ key : ") " ++ getDescription action) () $ Map.toList (getActions screen)

loadGame :: IO Screen
loadGame = do
    asciiLogo <- readFile "res/logo.txt"
    let startNewGameScreen = Screen "This will be the start screen." $
                                    Map.fromList [('q', Action "Quit" Nothing)]
    let logoScreen = Screen (asciiLogo ++ "\n\ta text-based adventure game by siliconbrain") $
                            Map.fromList [('n', Action "Start new game" (Just startNewGameScreen)),
                                          ('q', Action "Quit" Nothing)
                                         ]
    return logoScreen 
