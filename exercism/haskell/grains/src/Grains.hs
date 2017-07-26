module Grains (square, total) where

import Data.Bits  (shiftL)
import Data.Maybe (mapMaybe)

square :: Integer -> Maybe Integer
square n | n < 1     = Nothing
         | n > 64    = Nothing
         | otherwise = Just . shiftL 1 . fromInteger $ n - 1

total :: Integer
total = sum $ mapMaybe square [1 .. 64]
