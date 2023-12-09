module Main where

import Data.Foldable (foldl', traverse_)
import Data.List (nub)

readOasis :: String -> [[Int]]
readOasis = fmap (fmap read . words) . lines

diff :: [Int] -> [Int]
diff = zipWith (flip (-)) <*> tail

solve :: (Int -> Int -> Int) -> ([Int] -> Int) -> [[Int]] -> Int
solve f ht = sum . fmap (go [])
  where
    go xs ys
        | length (nub ys) == 1 = foldl' f (head ys) xs
        | otherwise = go (ht ys : xs) (diff ys)

part1 :: [[Int]] -> Int
part1 = solve (+) last

part2 :: [[Int]] -> Int
part2 = solve (flip (-)) head

main :: IO ()
main = do
    input <- readOasis <$> readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
