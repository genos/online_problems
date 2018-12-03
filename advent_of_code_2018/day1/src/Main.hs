module Main where

import Data.Foldable (find)
import Data.IntSet   (empty, insert, member)
import Data.List     (scanl')

readInt :: String -> Int
readInt ('+' : s) = read s
readInt s         = read s

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Maybe Int
part2 xs = fmap fst . find (uncurry member) $ zip list sets
 where
  list = scanl' (+) 0 . cycle $ xs
  sets = scanl' (flip insert) empty list

main :: IO ()
main = do
  xs <- fmap readInt . lines <$> readFile "input"
  print $ part1 xs
  print $ part2 xs
