module Clock (clockHour, clockMin, fromHourMin, toString) where

import Text.Printf (printf)

newtype Clock = C { _minutes :: Int } deriving (Eq)

_minsPerDay, _minsPerHr, _hrsPerDay :: Int
_minsPerDay = _minsPerHr * _hrsPerDay
_minsPerHr  = 60
_hrsPerDay  = 24

instance Num Clock where
  fromInteger   = fromHourMin 0 . fromInteger
  negate        = fromHourMin 0 . negate . _minutes
  (C x) + (C y) = fromHourMin 0 $ x + y
  (*)           = undefined
  abs           = undefined
  signum        = undefined

instance Show Clock where
  show = toString

clockHour :: Clock -> Int
clockHour = (`mod`_hrsPerDay) . (`div`_minsPerHr) . _minutes

clockMin :: Clock -> Int
clockMin = (`mod`_minsPerHr) . _minutes

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = C $ (_minsPerHr * hour + min) `mod` _minsPerDay

toString :: Clock -> String
toString = printf "%02d:%02d" <$> clockHour <*> clockMin
