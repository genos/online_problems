{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.List (subsequences)
import Data.Text (Text)
import Data.Text.IO qualified as T

testInput :: Text
testInput = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

parse_ :: Text -> [[Int]]
parse_ = either (error "Bad pasre") id . parseOnly ((line `sepBy1'` "\n") <* endOfInput)
  where
    line = decimal `sepBy1'` " "

isSafe :: [Int] -> Bool
isSafe xs = (monotonic xs && close xs) || (monotonic xs' && close xs')
  where
    xs' = reverse xs
    monotonic zs = and . zipWith (<) zs $ tail zs
    close zs = all ((< 4) . abs) . zipWith (-) zs $ tail zs

atMost1Drop :: [Int] -> [[Int]]
atMost1Drop xs = filter (\zs -> length zs >= n') (subsequences xs)
  where
    n' = pred $ length xs

part1, part2 :: [[Int]] -> Int
part1 = length . filter isSafe
part2 = length . filter (any isSafe . atMost1Drop)

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
