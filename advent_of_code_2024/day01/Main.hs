{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.IntMap qualified as I
import Data.List (sort)
import Data.Text (Text)
import Data.Text.IO qualified as T

parse_ :: Text -> ([Int], [Int])
parse_ = either (error "Bad pasre") unzip . parseOnly (line <* endOfInput)
  where
    line = ((,) <$> decimal <*> (skipSpace *> decimal)) `sepBy1'` "\n"

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) = sum . fmap abs $ zipWith (-) (sort xs) (sort ys)

part2 :: ([Int], [Int]) -> Int
part2 (xs, ys) = sum $ fmap get xs
  where
    zs = I.fromListWith (+) $ fmap (,1) ys
    get x = x * I.findWithDefault 0 x zs

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
