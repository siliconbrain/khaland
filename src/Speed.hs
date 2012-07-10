module Speed (
    Speed,
    fromKilometersPerHour,
    fromKilometersAndHours,
    fromMetersPerSecond,
    fromMetersAndSeconds,
    mkSpeed,
    getDistance,
    getTime,
) where

import Distance
import Time

newtype Speed a = MetersPerSecond a

fromMetersPerSecond :: (RealFrac a) => a -> Speed a
fromMetersPerSecond v = MetersPerSecond v

fromKilometersPerHour :: (RealFrac a) => a -> Speed a
fromKilometersPerHour v = MetersPerSecond $ v * (1000 / 3600)

fromMetersAndSeconds :: (RealFrac a) => a -> a -> Speed a
fromMetersAndSeconds d t = fromMetersPerSecond $ d / t

fromKilometersAndHours :: (RealFrac a) => a -> a -> Speed a
fromKilometersAndHours d t = fromKilometersPerHour $ d / t

mkSpeed :: (RealFrac a) => Distance a -> Time a -> Speed a
mkSpeed d t = fromMetersAndSeconds (toMeters d) (toSeconds t)

getTime :: (RealFrac a) => Distance a -> Speed a -> Time a
getTime d (MetersPerSecond v) = fromSeconds $ (toMeters d) / v

getDistance :: (RealFrac a) => Speed a -> Time a -> Distance a
getDistance (MetersPerSecond v) t = fromMeters $ v * (toSeconds t)

offset :: (RealFrac a) => Speed a -> Speed a -> Speed a
(MetersPerSecond v) `offset` (MetersPerSecond o) = MetersPerSecond $ v + o

multiply :: (RealFrac a) => Speed a -> a -> Speed a
(MetersPerSecond v) `multiply` m = MetersPerSecond $ v * m
