module Distance (
    Distance,
    fromKilometers,
    fromMeters,
    toKilometers,
    toMeters,
    offset,
    multiply,
) where

newtype Distance a = Meters a

fromKilometers :: (RealFrac a) => a -> Distance a
fromKilometers d = Meters $ d * 1000

fromMeters :: (RealFrac a) => a -> Distance a
fromMeters d = Meters d

toKilometers :: (RealFrac a) => Distance a -> a
toKilometers (Meters d) = d / 1000

toMeters :: (RealFrac a) => Distance a -> a
toMeters (Meters d) = d

offset :: (RealFrac a) => Distance a -> Distance a -> Distance a
(Meters d) `offset` (Meters o) = Meters $ d + o

multiply :: (RealFrac a) => Distance a -> a -> Distance a
(Meters d) `multiply` m = Meters $ d * m
