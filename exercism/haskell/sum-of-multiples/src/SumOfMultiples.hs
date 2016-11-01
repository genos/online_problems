module SumOfMultiples
  (sumOfMultiples)
  where

sumOfMultiples :: (Integral a) => [a] -> a -> a
sumOfMultiples fs n =
  sum . filter (\x -> 0 `elem` fmap (x `mod`) fs) $ [1 .. n - 1]
