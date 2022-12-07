module Main where

import Data.Char (ord)
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as I
import Data.List.Split (chunksOf)
import Data.Set (Set)
import qualified Data.Set as S

priority :: Set Char -> Int
priority = S.foldl' (\a b -> a + p b) 0
  where
    p = (m I.!) . ord
    m = I.fromList [(ord c, i) | (c, i) <- zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 ..]]

part1 :: [String] -> Int
part1 = sum . fmap (priority . inter)
  where
    inter line =
        let (xs, ys) = splitAt (length line `div` 2) line
         in S.fromList xs `S.intersection` S.fromList ys

part2 :: [String] -> Int
part2 = sum . fmap (priority . foldl1 S.intersection . fmap S.fromList) . chunksOf 3

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
