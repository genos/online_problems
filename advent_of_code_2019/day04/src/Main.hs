module Main where

import Control.Monad (ap)
import Data.Foldable (traverse_)
import Data.List     (group)

input :: [Int]
input = [273025 .. 767253]

digits :: Int -> [Int]
digits n = reverse $ ds n
 where
  ds 0 = []
  ds k = r : ds q where (q, r) = quotRem k 10

answer :: ([Int] -> Bool) -> Int
answer = length . flip filter (fmap digits input)

window2 :: [a] -> [(a, a)]
window2 = ap zip tail

twoAdjSame :: Eq a => [a] -> Bool
twoAdjSame = any (uncurry (==)) . window2

nonDecreasing :: Ord a => [a] -> Bool
nonDecreasing = all (uncurry (<=)) . window2

part1 :: Int
part1 = answer ((&&) <$> twoAdjSame <*> nonDecreasing)

hasAdjPair :: Eq a => [a] -> Bool
hasAdjPair = elem 2 . fmap length . group

part2 :: Int
part2 = answer ((&&) <$> nonDecreasing <*> hasAdjPair)

main :: IO ()
main = traverse_ print [part1, part2]
