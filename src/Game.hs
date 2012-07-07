module Game (
    GameState,
    Gender(..),
) where

data Gender = Male
            | Female

data GameState = GameState {
                   gender :: Gender
                 }
