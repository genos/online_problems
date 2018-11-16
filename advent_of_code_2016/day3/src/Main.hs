{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.FileEmbed        (embedFile)
import           Data.Foldable         (traverse_)
import           Data.List             (transpose)
import           Data.List.Split       (chunksOf)
import           Data.Maybe            (fromJust)

triple :: ByteString -> [Int]
triple =
  map (fst . fromJust . BC.readInt) . filter (/= "") . BC.splitWith (== ' ')

input :: ByteString
input = $(embedFile "input.txt")

triples :: [[Int]]
triples = map triple . BC.lines $ input

validTriangle :: [Int] -> Bool
validTriangle xs = (a + b) > c && (a + c) > b && (b + c) > a
  where [a, b, c] = take 3 xs

countValid :: [[Int]] -> Int
countValid = length . filter validTriangle

part1 :: Int
part1 = countValid triples

part2 :: Int
part2 = countValid . concatMap (chunksOf 3) . transpose $ triples

main :: IO ()
main = traverse_ print [part1, part2]
