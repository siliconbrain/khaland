module Time (
    Time,
    fromDays,
    fromHours,
    fromMinutes,
    fromSeconds,
    toDays,
    toHours,
    toMinutes,
    toSeconds,
    offset,
    multiply,
) where

daysToHours :: (RealFrac a) => a -> a
daysToHours = (24 *)

daysToMinutes :: (RealFrac a) => a -> a
daysToMinutes = hoursToMinutes . daysToHours

daysToSeconds :: (RealFrac a) => a -> a
daysToSeconds = minutesToSeconds . daysToMinutes

hoursToDays :: (RealFrac a) => a -> a
hoursToDays = (/ 24)

hoursToMinutes :: (RealFrac a) => a -> a
hoursToMinutes = (60 *)

hoursToSeconds :: (RealFrac a) => a -> a
hoursToSeconds = minutesToSeconds . hoursToMinutes

minutesToDays :: (RealFrac a) => a -> a
minutesToDays = hoursToDays . minutesToHours

minutesToHours :: (RealFrac a) => a -> a
minutesToHours = (/ 60)

minutesToSeconds :: (RealFrac a) => a -> a
minutesToSeconds = (60 *)

secondsToDays :: (RealFrac a) => a -> a
secondsToDays = hoursToDays . secondsToHours

secondsToHours :: (RealFrac a) => a -> a
secondsToHours = minutesToHours . secondsToMinutes

secondsToMinutes :: (RealFrac a) => a -> a
secondsToMinutes = (/ 60)

newtype Time a = Seconds a

fromDays :: (RealFrac a) => a -> Time a
fromDays t = Seconds $ daysToSeconds t

fromHours :: (RealFrac a) => a -> Time a
fromHours t = Seconds $ hoursToSeconds t

fromMinutes :: (RealFrac a) => a -> Time a
fromMinutes t = Seconds $ minutesToSeconds t

fromSeconds :: (RealFrac a) => a -> Time a
fromSeconds t = Seconds t

toDays :: (RealFrac a) => Time a -> a
toDays (Seconds t) = secondsToDays t

toHours :: (RealFrac a) => Time a -> a
toHours (Seconds t) = secondsToHours t

toMinutes :: (RealFrac a) => Time a -> a
toMinutes (Seconds t) = secondsToMinutes t

toSeconds :: (RealFrac a) => Time a -> a
toSeconds (Seconds t) = t

offset :: (RealFrac a) => Time a -> Time a -> Time a
(Seconds t) `offset` (Seconds o) = Seconds $ t + o

multiply :: (RealFrac a) => Time a -> a -> Time a
(Seconds t) `multiply` m = Seconds $ t * m
