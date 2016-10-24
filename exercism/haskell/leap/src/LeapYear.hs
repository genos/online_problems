module LeapYear
  (isLeapYear)
  where

isLeapYear :: Integer -> Bool
isLeapYear y = (y `mod` 4 == 0) && (y `mod` 100 /= 0 || y `mod` 400 == 0)
