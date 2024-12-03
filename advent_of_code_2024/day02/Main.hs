{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.List (subsequences)
import Data.Text (Text)
import Data.Text.IO qualified as T

parse_ :: Text -> [[Int]]
parse_ = either (error "Bad parse") id . parseOnly ((line `sepBy1'` "\n") <* endOfInput)
  where
    line = decimal `sepBy1'` " "

isSafe :: [Int] -> Bool
isSafe xs = close xs && (monotonic xs || monotonic (reverse xs))
  where
    pair f zs = zipWith f zs $ drop 1 zs
    monotonic = and . pair (<)
    close = all ((< 4) . abs) . pair (-)

dropAtMost1 :: [Int] -> [[Int]]
dropAtMost1 xs = filter (\zs -> length zs >= pred (length xs)) (subsequences xs)

part1, part2 :: [Int] -> Bool
part1 = isSafe
part2 = any isSafe . dropAtMost1

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . (\p -> length $ filter p input)) [part1, part2]
