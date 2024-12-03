{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.List (sort)
import Data.Text (Text)
import Data.Text.IO qualified as T

testInput :: Text
testInput = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3\n"

readL :: Text -> ([Int], [Int])
readL = either (error "Bad pasre") unzip . parseOnly ((pair `sepBy1` "\n") <* skipSpace <* endOfInput)
  where
    pair = (,) <$> decimal <*> (skipSpace *> decimal)

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) = sum . fmap abs $ zipWith (-) (sort xs) (sort ys)

main :: IO ()
main = print . part1 . readL =<< T.readFile "input.txt"
