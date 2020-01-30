module Main where

import Data.Char (digitToInt)
import Data.Foldable (foldl', traverse_)
import Data.List (scanl')

input :: IO [Int]
input = fmap digitToInt <$> readFile "input"

horner :: [Int] -> Int
horner = foldl' ((+) . (10 *)) 0

fin :: [[Int]] -> Int
fin = horner . take 8 . (!! 100)

part1 :: [Int] -> Int
part1 signal = fin $ iterate step signal
  where
    step xs = take (length signal) $ fmap (dot xs . expand) [1 ..]
    dot xs = (`rem` 10) . abs . sum . zipWith (*) xs
    expand n = tail . cycle $ concatMap (replicate n) [0, 1, 0, -1]

-- https://git.sr.ht/~quf/advent-of-code-2019/tree/master/16/16-2.hs (see also rust version)
part2 :: [Int] -> Int
part2 signal =
  if offset <= length signal `div` 2
    then error "offset too small"
    else fin $ iterate step signal'
  where
    offset = horner $ take 7 signal
    signal' = drop offset . concat $ replicate 10000 signal
    step = fmap ((`mod` 10) . abs) . reverse . cumSum . reverse
    cumSum = tail . scanl' (+) 0

main :: IO ()
main = do
  signal <- input
  traverse_ print [part1 signal, part2 signal]
