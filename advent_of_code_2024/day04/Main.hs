module Main where

import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Linear.V2 (V2 (..))

type Coord = V2 Int
type Dir = V2 Int
type Board = Map Coord Char

parse :: String -> Board
parse input = M.fromList [(V2 i j, c) | (i, l) <- zip [0 ..] $ lines input, (j, c) <- zip [0 ..] l]

part1 :: Board -> Int
part1 b = sum $ [countXMAS (V2 i j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]
  where
    countXMAS dir = M.size $ M.filterWithKey (\xy _ -> and $ zipWith is (line xy dir) "XMAS") b
    line xy dir = take 4 $ iterate (+ dir) xy
    is xy c = b M.!? xy == Just c

part2 :: Board -> Int
part2 b = sum $ fmap countX_MAS [(V2 1 0, V2 0 1), (V2 0 1, V2 1 0), (V2 (-1) 0, V2 0 (-1)), (V2 0 (-1), V2 (-1) 0)]
  where
    countX_MAS (d1, d2) =
        M.size $
            M.filterWithKey
                (\xy _ -> and [xy `is` 'M', (xy + 2 * d1) `is` 'M', (xy + d1 + d2) `is` 'A', (xy + 2 * d2) `is` 'S', (xy + 2 * (d1 + d2)) `is` 'S'])
                b
    is xy c = b M.!? xy == Just c

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
