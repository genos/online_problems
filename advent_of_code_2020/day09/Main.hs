{-# LANGUAGE BangPatterns #-}
module Main
  ( main
  ) where

import qualified Data.IntSet         as I
import qualified Data.Vector.Unboxed as V

input :: IO [Int]
input = rights . fmap (readEither . toString) . lines <$> readFileText "input.txt"

-- https://stackoverflow.com/a/27733778
windows :: Int -> [a] -> [[a]]
windows n = getZipList . traverse ZipList . take n . tails

part1 :: [Int] -> Maybe Int
part1 xmas = viaNonEmpty (fst . head) . filter (not . f) $ zip (drop 25 xmas)
                                                               (windows 25 xmas)
 where
  f (x, xs) =
    let ys = fromList xs in I.foldl' (\b y -> b || I.member (x - y) ys) False ys

bounds :: V.Vector Int -> Int -> (Int, Int)
bounds v goal = go 0 1
 where
  go !lo !hi = case compare ((v V.! hi) - (v V.! lo)) goal of
    LT -> go lo (hi + 1)
    EQ -> (lo, hi)
    GT -> go (lo + 1) hi

part2 :: [Int] -> Maybe Int
part2 xmas = do
  goal <- part1 xmas
  let v        = V.fromList xmas
      (lo, hi) = bounds (V.scanl' (+) 0 $ V.fromList xmas) goal
      xs       = V.slice lo (hi - lo) v
  pure $ (+) <$> V.minimum <*> V.maximum $ xs

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
