module Main where

import Data.Char (digitToInt)
import Data.Foldable (foldl')

input :: IO [Int]
input = fmap digitToInt <$> readFile "input"

expand :: Int -> [Int]
expand n = tail . cycle $ concatMap (replicate n) [0, 1, 0, -1]

dot :: [Int] -> [Int] -> Int
dot list pattern = (`rem` 10) . abs . sum $ zipWith (*) list pattern

step :: [Int] -> [Int]
step list = take (length list) $ fmap (dot list . expand) [1 ..]

toInt :: [Int] -> Int
toInt = foldl' ((+) . (10 *)) 0

part1 :: [Int] -> Int
part1 = toInt . take 8 . (!! 100) . iterate step

main :: IO ()
main = print =<< part1 <$> input
