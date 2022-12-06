module Main where

import Data.Foldable (traverse_)
import Data.List (findIndex, nub, tails)

startOf :: Int -> String -> Maybe Int
startOf n = fmap (n +) . findIndex ((n ==) . length . nub) . fmap (take n) . tails

main :: IO ()
main = do
    input <- readFile "input.txt"
    traverse_ (print . ($ input)) [startOf 4, startOf 14]
