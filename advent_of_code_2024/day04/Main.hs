module Main where

import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Tuple (swap)
import Linear.V2 (V2 (..))

type Coord = V2 Int
type Dir = V2 Int
type Board = Map Coord Char

parse :: String -> Board
parse input = M.fromList [(V2 i j, c) | (i, l) <- zip [0 ..] $ lines input, (j, c) <- zip [0 ..] l]

is :: Board -> Coord -> Char -> Bool
is b xy c = b M.!? xy == Just c

count :: Board -> a -> (Coord -> a -> [Coord]) -> String -> Int
count b ds f str = M.size $ M.filterWithKey (\xy _ -> and $ zipWith (is b) (f xy ds) str) b

part1 :: Board -> Int
part1 b = sum $ [countXMAS (V2 i j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]
  where
    countXMAS dir = count b dir line "XMAS"
    line xy dir = take 4 $ iterate (+ dir) xy

part2 :: Board -> Int
part2 b = sum . fmap countX_MAS $ pairs <> fmap swap pairs
  where
    pairs = [(V2 1 0, V2 0 1), (V2 (-1) 0, V2 0 (-1))]
    countX_MAS (d1, d2) = count b (d1, d2) bigX "MMASS"
    bigX xy (d1, d2) = [xy, xy + 2 * d1, xy + d1 + d2, xy + 2 * d2, xy + 2 * (d1 + d2)]

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
