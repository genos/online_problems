module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Eq a, Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | any (<= 0) [a, b, c]                       =  Illegal
  | (a + b < c) || (a + c < b) || (b + c < a)  =  Illegal
  | (a == b) && (b == c)                       =  Equilateral
  | (a == b) || (a == c) || (b == c)           =  Isosceles
  | otherwise                                  =  Scalene
