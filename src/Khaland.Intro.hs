module Khaland.Intro (
    playIntro
  ) where

import Control.Concurrent (threadDelay)

import Khaland.Content (readContentFile)

import System.Console.ANSI (clearScreen)

data Screen = Screen (Maybe Int) [String]

-- |Play intro sequence
playIntro :: IO ()
playIntro = do
    introFileContents <- readContentFile "intro.txt"
    playScreens . getScreens . lines $ introFileContents

playScreens :: [Screen] -> IO ()
playScreens [] = return ()
playScreens (Screen control content:rest) = do
    clearScreen
    mapM_ putStrLn content  -- write content lines to screen
    case control of
        Nothing -> getLine >> return () -- might have to put a "Press ENTER to continue" text just to clarify the situation
        Just interval -> threadDelay interval
    playScreens rest

getScreens :: [String] -> [Screen]
getScreens xs = case getScreen xs of
    Nothing -> []
    Just (screen, rest) -> screen : (getScreens rest)

getScreen :: [String] -> Maybe (Screen,[String])
getScreen [] = Nothing
getScreen (l:ls) = Just (Screen control content, rest)
    where
        control = if null l then Nothing else Just $ read l
        (content, rest') = span (not . null) ls  -- split list at first empty line
        rest = drop 1 rest'  -- drop empty line