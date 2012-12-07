module Character (
    Gender(..),
    Player,
    travelSpeed,
) where

import Speed

data Gender = Male
            | Female
    deriving Eq

data Player = Player {
                  gender :: Gender,
                  travelSpeed :: Speed Float
              }
