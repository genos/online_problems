module Main where

import Data.Foldable (traverse_)
import Data.IntMultiSet (IntMultiSet)
import Data.IntMultiSet qualified as IM
import Data.NumberLength (numberLength)

parse :: String -> IntMultiSet
parse = IM.fromList . fmap read . words

blinkAll :: IntMultiSet -> IntMultiSet
blinkAll = IM.concatMap blink
  where
    blink 0 = [1]
    blink k | even d = [l, r]
      where
        d = numberLength k
        (l, r) = k `divMod` (10 ^ (d `div` 2))
    blink k = [2024 * k]

solve :: Int -> IntMultiSet -> Int
solve n = IM.size . (!! n) . iterate blinkAll

part1, part2 :: IntMultiSet -> Int
part1 = solve 25
part2 = solve 75

main :: IO ()
main = do
    m <- parse <$> readFile "input.txt"
    traverse_ (print . ($ m)) [part1, part2]
