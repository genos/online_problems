module Main where

import Data.Char (ord)
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as I
import qualified Data.IntSet as IS
import Data.List.Split (chunksOf)

parse :: [String] -> [[Int]]
parse = fmap (fmap p)
  where
    p = (m I.!) . ord
    m = I.fromList [(ord c, i) | (c, i) <- zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 ..]]

-- I think GHC cleverly makes `t` into `Identity` and removes it!
solve :: (Foldable t, Functor t) => (a -> t [Int]) -> [a] -> Int
solve f = sum . fmap (intersect . f)
  where
    intersect = IS.findMin . foldl1 IS.intersection . fmap IS.fromList

part1 :: [[Int]] -> Int
part1 = solve (\ps -> chunksOf (length ps `div` 2) ps)

part2 :: [[Int]] -> Int
part2 = solve id . chunksOf 3

main :: IO ()
main = do
    input <- parse . lines <$> readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
