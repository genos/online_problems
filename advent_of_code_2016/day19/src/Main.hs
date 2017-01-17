{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import Data.Sequence (Seq)
import qualified Data.Sequence as S

type Algebra f a = f a -> a

build :: Int -> Seq Int
build n = S.fromList [1 .. n]

part1 :: Int -> Int
part1 = f . build where
  f :: Algebra Seq Int
  f xs | S.null xs      = -1
       | length xs <= 2 = S.index xs 0
       | otherwise      = f $ S.drop 2 xs S.>< S.take 1 xs

testInput :: Int
testInput = 5

input :: Int
input = 3014387

testPart1 :: Bool
testPart1 = 3 == part1 testInput

part2 :: Int -> Int
part2 = f . build where
  f :: Algebra Seq Int
  f xs | S.null xs      = -1
       | length xs <= 2 = S.index xs 0
       | otherwise      = f ws where
                          ws = zs S.|> z
                          (z S.:< zs) = S.viewl ys
                          ys = S.take n xs S.>< S.drop (n + 1) xs
                          n  = length xs `div` 2

testPart2 :: Bool
testPart2 = 2 == part2 testInput

main :: IO ()
main = do
    print testPart1
    print . part1 $ input
    print testPart2
    print . part2 $ input
