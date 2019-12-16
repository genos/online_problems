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

toInt :: Foldable f => f Int -> Int
toInt = foldl' ((+) . (10 *)) 0

part1 :: IO Int
part1 = toInt . take 8 . (!! 100) . iterate step <$> input

part2 :: IO Int
part2 = toInt . take 8 . from7th . (!! 100) . iterate step <$> signal
  where
    signal = concat . replicate 1000 <$> input
    from7th xs = drop (toInt $ take 7 xs) xs

main :: IO ()
main = do
  print =<< part1
  print =<< part2
