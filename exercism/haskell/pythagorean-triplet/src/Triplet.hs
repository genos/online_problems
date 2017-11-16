module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = x * x + y * y == z * z
  where [x, y, z] = sort [a, b, c]

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet = (,,)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor =
  [ (a, b, c)
  | a <- [minFactor .. maxFactor]
  , b <- [a .. maxFactor]
  , c <- [b .. maxFactor]
  , isPythagorean (a, b, c)
  ]
