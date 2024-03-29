module Main where

import Data.Foldable (traverse_)

countGT :: [Int] -> [Int] -> Int
countGT xs = length . filter id . zipWith (<) xs

part1 :: [Int] -> Int
part1 = countGT <*> tail

part2 :: [Int] -> Int
part2 = countGT <*> drop 3

main :: IO ()
main = do
  depths <- fmap read . lines <$> readFile "input.txt"
  traverse_ (print . ($ depths)) [part1, part2]
