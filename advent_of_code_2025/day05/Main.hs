{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.IntSet qualified as I
import Data.Text (Text)
import Data.Text.IO qualified as T

parse_ :: Text -> ([(Int, Int)], [Int])
parse_ = either (error "Bad parse") id . parseOnly twoSections
  where
    twoSections = (,) <$> (range `sepBy1'` char '\n') <*> ("\n\n" *> (ingredient `sepBy1'` char '\n'))
    range = (,) <$> decimal <*> (char '-' *> decimal)
    ingredient = decimal

part1 :: [(Int, Int)] -> [Int] -> Int
part1 ranges = length . filter (isFresh ranges)
  where
    isFresh rs i = any (\(lo, hi) -> lo <= i && i <= hi) rs

part2 :: [(Int, Int)] -> Int
part2 = I.size . foldl' f I.empty
  where
    f s = I.union s . I.fromRange

main :: IO ()
main = do
    (ranges, ingredients) <- parse_ <$> T.readFile "input.txt"
    print $ part1 ranges ingredients
    print $ part2 ranges
