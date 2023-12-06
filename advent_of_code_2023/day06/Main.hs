{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Text.IO qualified as T

test :: Text
test = "Time:      7  15   30\nDistance:  9  40  200"

readRaces :: Text -> [(Word, Word)]
readRaces = either (error "Bad parse") id . parseOnly (zip <$> time <*> ("\n" *> distance) <* endOfInput)
  where
    time = "Time:" *> skipSpace *> nums
    distance = "Distance:" *> skipSpace *> nums
    nums = decimal `sepBy1'` skipSpace

part1 :: [(Word, Word)] -> Int
part1 = product . fmap (\(t, d) -> length $ filter (beats d t) [0 .. t])
  where
    beats distance time t = (time - t) * t > distance

main :: IO ()
main = do
    print $ readRaces test
    print . part1 $ readRaces test
    input <- readRaces <$> T.readFile "input.txt"
    print . part1 $ input
