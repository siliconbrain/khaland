module Character (
    Gender(..),
    Player,
    travelSpeed,
) where

import Speed

data Gender = Male
            | Female

data Player = Player {
                  gender :: Gender,
                  travelSpeed :: Speed Float
              }
