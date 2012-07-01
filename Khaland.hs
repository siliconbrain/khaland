import Game
import System.IO

gameLoop :: Screen -> IO ()
gameLoop screen = do
    putScreen screen
    input <- getLine
    case processInput input screen of
        Left inputError -> do
            putStrLn inputError
            getLine
            gameLoop screen
        Right Nothing -> return ()
        Right (Just newScreen) -> gameLoop newScreen

main :: IO ()
main = do
    startScreen <- loadGame
    gameLoop startScreen
    putStrLn "The End"
