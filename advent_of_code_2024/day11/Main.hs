module Main where

import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.NumberLength (numberLength)

parse :: String -> IntMap Int
parse = IM.fromListWith (+) . fmap ((,1) . read) . words

blink :: Int -> [Int]
blink 0 = [1]
blink n | even d = [l, r]
  where
    d = numberLength n
    (l, r) = n `divMod` (10 ^ (d `div` 2))
blink n = [2024 * n]

blinkAll :: IntMap Int -> IntMap Int
blinkAll = IM.fromListWith (+) . concatMap (\(k, v) -> (,v) <$> blink k) . IM.toList

solve :: Int -> IntMap Int -> Int
solve n = sum . (!! n) . iterate blinkAll

part1, part2 :: IntMap Int -> Int
part1 = solve 25
part2 = solve 75

main :: IO ()
main = do
    m <- parse <$> readFile "input.txt"
    traverse_ (print . ($ m)) [part1, part2]
