{-# LANGUAGE BangPatterns #-}
module PrimeFactors (primeFactors) where

-- When in doubt, trial division
primeFactors :: Integer -> [Integer]
primeFactors n | n < 2     = []
               | even n    = 2 : primeFactors (n `div` 2)
               | otherwise = reverse $ go 3 n []
 where
  go !k !m !fs | k > m          = fs
               | k == m         = k : fs
               | m `mod` k == 0 = go k (m `div` k) (k : fs)
               | otherwise      = go (k + 2) m fs
