module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c | x <= 0 || x + y < z = Illegal
                   | x == z              = Equilateral
                   | x == y || y == z    = Isosceles
                   | otherwise           = Scalene
  where [x, y, z] = sort [a, b, c]
