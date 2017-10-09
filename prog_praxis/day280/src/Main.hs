module Main where

import Data.Numbers.Primes (isPrime, primes)

sumPrimesMod :: Int -> Bool
sumPrimesMod n = isPrime (sum (take n primes) `mod` n)

main :: IO ()
main = do
  let ns = filter sumPrimesMod [1 .. 367]
  print ns
  print $ length ns
