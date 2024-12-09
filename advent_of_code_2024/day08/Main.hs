module Main where

import Data.Foldable (traverse_)
import Data.List (nub, sort)
import Data.Map.Strict qualified as M
import Linear.V2 (V2 (..))
import Linear.Vector ((*^))

type Map = M.Map (V2 Int) Char

parse :: String -> Map
parse input =
    M.fromList
        [ (V2 j i, c)
        | (j, l) <- zip [0 ..] $ reverse $ lines input
        , (i, c) <- zip [0 ..] l
        ]

solve :: [Int] -> Map -> Int
solve ns m = length . nub . sort . concatMap antinodes . M.keys $ M.filter (/= '.') m
  where
    antinodes u = concatMap (f u) $ filter (/= u) . M.keys $ M.filter (== (m M.! u)) m
    f u v = takeWhile ok [v + i *^ (v - u) | i <- ns]
    ok (V2 x y) = 0 <= x && x <= xHi && 0 <= y && y <= yHi
    V2 xHi yHi = fst $ M.findMax m

part1 :: Map -> Int
part1 = solve [1]

part2 :: Map -> Int
part2 = solve [0 ..]

main :: IO ()
main = do
    m <- parse <$> readFile "input.txt"
    traverse_ (print . ($ m)) [part1, part2]
