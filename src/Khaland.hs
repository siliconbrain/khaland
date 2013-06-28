import Khaland.Game
import Khaland.Interactive
import Khaland.Intro
import Khaland.Menu

main :: IO ()
main = do
    playIntro
    command <- showMainMenu
    case command of
        ExitGame           -> return ()  -- do nothing
        PlayGame gameState -> playGame gameState
    -- should print some goodbye image here
    putStrLn ""
    putStrLn "################################"
    putStrLn "# In memoriam Szalontai Andor. #"
    putStrLn "################################"
    putStrLn ""
    putStrLn "Thanks for playing!"
