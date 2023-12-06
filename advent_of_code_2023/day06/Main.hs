{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

test :: Text
test = "Time:      7  15   30\nDistance:  9  40  200"

type Race = (Word, Word)

solve :: (Text -> [Race]) -> Text -> Int
solve reader = product . fmap (\(t, d) -> length $ filter (beats d t) [0 .. t]) . reader
  where
    beats distance time t = (time - t) * t > distance

part1 :: Text -> Int
part1 = solve read1
  where
    read1 = either (error "Bad parse") id . parseOnly (zip <$> time <*> ("\n" *> distance) <* endOfInput)
    time = "Time:" *> skipSpace *> nums
    distance = "Distance:" *> skipSpace *> nums
    nums = decimal `sepBy1'` skipSpace

part2 :: Text -> Int
part2 = solve read2
  where
    read2 = either (error "Bad parse") id . parseOnly ((\t d -> [(t, d)]) <$> time <*> ("\n" *> distance) <* endOfInput)
    time = "Time:" *> skipSpace *> num
    distance = "Distance:" *> skipSpace *> num
    num :: Parser Word
    num = read . T.unpack . T.filter (/= ' ') <$> takeTill isEndOfLine

main :: IO ()
main = do
    traverse_ (print . ($ test)) [part1, part2]
    input <- T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
