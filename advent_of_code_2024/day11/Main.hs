module Main where

import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.NumberLength (numberLength)

parse :: String -> IntMap Int
parse = IM.fromListWith (+) . fmap ((,1) . read) . words

blinkAll :: IntMap Int -> IntMap Int
blinkAll = IM.foldlWithKey' (\m k v -> IM.unionWith (+) m $ blink k v) IM.empty
  where
    blink 0 v = IM.singleton 1 v
    blink k v | even d = IM.fromListWith (+) [(l, v), (r, v)]
      where
        d = numberLength k
        (l, r) = k `divMod` (10 ^ (d `div` 2))
    blink k v = IM.singleton (2024 * k) v

solve :: Int -> IntMap Int -> Int
solve n = sum . (!! n) . iterate blinkAll

part1, part2 :: IntMap Int -> Int
part1 = solve 25
part2 = solve 75

main :: IO ()
main = do
    m <- parse <$> readFile "input.txt"
    traverse_ (print . ($ m)) [part1, part2]
