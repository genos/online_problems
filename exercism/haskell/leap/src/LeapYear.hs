module LeapYear
  (isLeapYear)
  where

isLeapYear :: Integer -> Bool
isLeapYear y
  | modZero 400 = True
  | modZero 100 = False
  | modZero 4   = True
  | otherwise   = False
    where
      modZero n = y `mod` n == 0
