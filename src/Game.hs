module Game (
    GameState,
) where

import Character
import Control.Monad.State
import qualified Distance as Dst
import qualified Speed as Spd
import qualified Time as Tm

data GameState = GameState {
                     player :: Player,
                     currentTime :: Tm.Time Float
                 }

travel dist = do
    gameState <- get
    let playerChar = player gameState
    put $ GameState playerChar ((currentTime gameState) `Tm.offset` (Spd.getTime dist (travelSpeed playerChar)))


