{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import Data.Sequence (Seq)
import qualified Data.Sequence as S

type Elves = Seq Int

elves :: Int -> Elves
elves n = S.fromList [1 .. n]

part1 :: Int -> Maybe Int
part1 = go . elves
  where
    go :: Elves -> Maybe Int
    go xs
      | S.null xs = Nothing
      | length xs <= 2 = Just $ S.index xs 0
      | otherwise = go $ S.drop 2 xs S.>< S.take 1 xs

testInput :: Int
testInput = 5

input :: Int
input = 3014387

testPart1 :: Bool
testPart1 = Just 3 == part1 testInput

part2 :: Int -> Maybe Int
part2 = go . elves
  where
    go :: Elves -> Maybe Int
    go xs
      | S.null xs = Nothing
      | length xs <= 2 = Just $ S.index xs 0
      | otherwise = go $ S.drop 1 ys S.>< S.take 1 ys
      where
        ys = S.take n xs S.>< S.drop (n + 1) xs
        n = length xs `div` 2

testPart2 :: Bool
testPart2 = Just 2 == part2 testInput

main :: IO ()
main = do
    print testPart1
    print . part1 $ input
    print testPart2
    print . part2 $ input
