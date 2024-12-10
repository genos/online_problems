module Main where

import Data.Char (digitToInt)
import Data.Foldable (traverse_)
import Data.List (nub, sort)
import Data.Map.Strict qualified as M
import Linear.V2 (V2 (..))

type Coord = V2 Int
type Map = M.Map Coord Int

parse :: String -> Map
parse input =
    M.fromList
        [ (V2 j i, digitToInt c)
        | (j, l) <- zip [0 ..] $ reverse $ lines input
        , (i, c) <- zip [0 ..] l
        ]

solve :: ([Coord] -> [Coord]) -> Map -> Int
solve f m = sum . fmap (length . f . walk) . M.keys $ M.filter (== 0) m
  where
    walk p = if m M.! p == 9 then [p] else concatMap walk (steps p)
    steps p =
        [ q
        | let d = m M.! p
        , q <- [p + e | e <- [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]]
        , m M.!? q == Just (d + 1)
        ]

part1 :: Map -> Int
part1 = solve (nub . sort)

part2 :: Map -> Int
part2 = solve id

main :: IO ()
main = do
    m <- parse <$> readFile "input.txt"
    traverse_ (print . ($ m)) [part1, part2]
