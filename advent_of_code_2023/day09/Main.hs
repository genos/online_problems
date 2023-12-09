module Main where

import Data.List (nub)

test1 :: String
test1 = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

readOasis :: String -> [[Int]]
readOasis = fmap (fmap read . words) . lines

diff :: [Int] -> [Int]
diff = zipWith (flip (-)) <*> tail

part1 :: [[Int]] -> Int
part1 = sum . fmap (next [])
  where
    next xs ys
        | length (nub ys) == 1 = head ys + sum xs
        | otherwise = next (last ys : xs) (diff ys)

main :: IO ()
main = do
    print . part1 $ readOasis test1
    input <- readOasis <$> readFile "input.txt"
    print $ part1 input
