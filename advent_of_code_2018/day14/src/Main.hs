{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.Foldable (foldl')
import           Data.List     (isPrefixOf, tails)
import qualified Data.Sequence as S

digits :: Int -> [Int]
digits = go []
 where
  go !ds !n | q <= 0    = ds'
            | otherwise = go ds' q
   where
    (!q, !r) = n `divMod` 10
    !ds'     = r : ds

undigits :: [Int] -> Int
undigits = foldl' (\x y -> 10 * x + y) 0

recipes :: [Int]
recipes = 3 : 7 : go 0 1 (S.fromList [3, 7])
 where
  go !elf1 !elf2 !scores = toPrepend ++ go elf1' elf2' scores'
   where
    toPrepend = digits (score1 + score2)
    score1    = scores `S.index` elf1
    score2    = scores `S.index` elf2
    scores'   = scores S.>< S.fromList toPrepend
    elf1'     = (elf1 + score1 + 1) `mod` S.length scores'
    elf2'     = (elf2 + score2 + 1) `mod` S.length scores'


part1 :: Int -> Int
part1 n = undigits . take 10 $ drop n recipes

part1Tests :: Bool
part1Tests = and $ zipWith (\n e -> e == part1 n)
                           [9, 5, 18, 2018]
                           [5158916779, 124515891, 9251071085, 5941429882]

part2 :: Int -> Int
part2 n =
  let !ds = digits n
  in  length . takeWhile (not . (ds `isPrefixOf`)) $ tails recipes

part2Tests :: Bool
part2Tests = and $ zipWith (\n e -> e == part2 n)
                           [51589, 1245, 92510, 59414]
                           [9, 6, 18, 2018]

main :: IO ()
main = do
  print part1Tests
  print $ part1 824501
  print part2Tests
  print $ part2 824501
