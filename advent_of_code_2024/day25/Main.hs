module Main where

import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.List.Split (splitOn)

type Lock = [Int]
type Key = [Int]

parse :: String -> ([Lock], [Key])
parse = foldl' f ([], []) . splitOn "\n\n"
  where
    f (locks, keys) s = if isL then (ints ls : locks, keys) else (locks, ints (reverse ls) : keys)
      where
        isL = '#' `elem` takeWhile (/= '\n') s
        ls = lines s
        ints = foldl' (zipWith (+)) (replicate 5 (-1)) . fmap (fmap (bool 0 1 . (== '#')))

part1 :: ([Lock], [Key]) -> Int
part1 (locks, keys) = sum [1 | l <- locks, k <- keys, (< 6) . maximum $ zipWith (+) l k]

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    traverse_ (print . ($ input)) [part1]
