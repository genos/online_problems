{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

toNum :: (String -> Maybe Int) -> String -> Int
toNum f s = 10 * head ns + last ns where ns = mapMaybe f $ tails s

solve :: (String -> Maybe Int) -> String -> Int
solve f = sum . fmap (toNum f) . lines

part1 :: String -> Int
part1 = solve (\case (c : _) | isDigit c -> Just $ digitToInt c; _ -> Nothing)

part2 :: String -> Int
part2 = solve f
  where
    f input@(c : _)
        | isDigit c = Just $ digitToInt c
        | "one" `isPrefixOf` input = Just 1
        | "two" `isPrefixOf` input = Just 2
        | "three" `isPrefixOf` input = Just 3
        | "four" `isPrefixOf` input = Just 4
        | "five" `isPrefixOf` input = Just 5
        | "six" `isPrefixOf` input = Just 6
        | "seven" `isPrefixOf` input = Just 7
        | "eight" `isPrefixOf` input = Just 8
        | "nine" `isPrefixOf` input = Just 9
    f _ = Nothing

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ part1 input
    print $ part2 input
