{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (digitToInt, isNumber)
import Data.Foldable (foldl')

p1Test :: String
p1Test = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

firstLast :: [a] -> [a]
firstLast t = head t : [last t]

part1 :: String -> Int
part1 = sum . fmap (foldl' ((+) . (* 10)) 0 . fmap digitToInt . firstLast . filter isNumber) . lines

p2Test :: String
p2Test = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

main :: IO ()
main = do
    print $ part1 p1Test
    input <- readFile "input.txt"
    print $ part1 input
