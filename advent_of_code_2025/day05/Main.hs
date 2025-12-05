{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Text
import Data.List (partition)
import Data.Text (Text)
import Data.Text.IO qualified as T

parse_ :: Text -> ([(Int, Int)], [Int])
parse_ = either (error "Bad parse") id . parseOnly ((,) <$> ranges <*> ("\n\n" *> ingredients))
  where
    ranges = ((,) <$> decimal <*> (char '-' *> decimal)) `sepBy1'` char '\n'
    ingredients = decimal `sepBy1'` char '\n'

part1 :: [(Int, Int)] -> [Int] -> Int
part1 ranges = length . filter (\i -> any (\(a, b) -> a <= i && i <= b) ranges)

part2 :: [(Int, Int)] -> Int
part2 = sum . map (\(a, b) -> b - a + 1) . foldl' merge []
  where
    merge merged (a, b) =
        let (disjoint, overlaps) = partition (\(x, y) -> x > b || y < a) merged
            new = foldl' (\(x0, y0) (x1, y1) -> (min x0 x1, max y0 y1)) (a, b) overlaps
         in new : disjoint

main :: IO ()
main = do
    (ranges, ingredients) <- parse_ <$> T.readFile "input.txt"
    print $ part1 ranges ingredients
    print $ part2 ranges
