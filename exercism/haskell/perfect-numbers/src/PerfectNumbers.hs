module PerfectNumbers (classify, Classification(..)) where

import Math.NumberTheory.ArithmeticFunctions

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n | n < 1  = Nothing
           | a < n  = Just Deficient
           | a == n = Just Perfect
           | a > n  = Just Abundant
  where a = subtract n . sum . divisors $ n
