module Main where

import Data.Foldable (foldl', traverse_)
import Data.List (nub)

readOasis :: String -> [[Int]]
readOasis = fmap (fmap read . words) . lines

solve :: ([Int] -> [Int] -> Int) -> [[Int]] -> Int
solve f = sum . fmap (f [])

sub :: Int -> Int -> Int
sub = flip (-)

diff :: [Int] -> [Int]
diff = zipWith sub <*> tail

part1 :: [[Int]] -> Int
part1 = solve next
  where
    next xs ys
        | length (nub ys) == 1 = head ys + sum xs
        | otherwise = next (last ys : xs) (diff ys)

part2 :: [[Int]] -> Int
part2 = solve prev
    where
        prev xs ys 
          | length (nub ys) == 1 = foldl' sub (head ys) xs
          | otherwise = prev (head ys : xs) (diff ys)

main :: IO ()
main = do
    input <- readOasis <$> readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
