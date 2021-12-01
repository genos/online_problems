module Main where

import Data.Bool     (bool)
import Data.Foldable (traverse_)

countDiff :: [Int] -> Int
countDiff = sum . fmap (bool 1 0 . (>= 0)) . (zipWith (-) <*> tail)

part1 :: [Int] -> Int
part1 = countDiff

part2 :: [Int] -> Int
part2 depths =
  countDiff $ zipWith3 (\a b c -> a + b + c) depths (tail depths) (drop 2 depths)

main :: IO ()
main = do
  depths <- fmap read . lines <$> readFile "input.txt"
  traverse_ (print . ($ depths)) [part1, part2]
