module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = a ^ 2 + b ^ 2 + c ^ 2 == 2 * (maximum [a, b, c]) ^ 2

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
