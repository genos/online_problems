module Clock (clockHour, clockMin, fromHourMin, toString) where

import Text.Printf (printf)

newtype Clock = C { _minutes :: Int } deriving (Eq)

_minsPerDay, _minsPerHr, _hrsPerDay :: Int
_minsPerDay = _minsPerHr * _hrsPerDay
_minsPerHr  = 60
_hrsPerDay  = 24

instance Num Clock where
  fromInteger n = C $ fromIntegral n
  negate (C m)  = C $ (-m) `mod` _minsPerDay
  (C x) + (C y) = C $ x + y `mod` _minsPerDay
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
