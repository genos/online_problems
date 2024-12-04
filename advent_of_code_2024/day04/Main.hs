module Main where

import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Linear.V2 (V2 (..))

parse :: String -> Map (V2 Int) Char
parse input = M.fromList [(V2 i j, c) | (i, l) <- zip [0 ..] $ lines input, (j, c) <- zip [0 ..] l]

countXMAS :: Map (V2 Int) Char -> V2 Int -> Int
countXMAS board direction = M.size $ M.filterWithKey (\xy _ -> and $ zipWith is (line xy) "XMAS") board
  where
    is xy c = board M.!? xy == Just c
    line xy = take 4 $ iterate (+ direction) xy

part1 :: Map (V2 Int) Char -> Int
part1 board = sum $ [countXMAS board (V2 i j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  traverse_ (print . ($ input)) [part1]
