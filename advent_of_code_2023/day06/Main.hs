{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

test :: Text
test = "Time:      7  15   30\nDistance:  9  40  200"

solve :: Parser [Word] -> Text -> Int
solve parser = product . fmap (\(t, d) -> length $ filter (beats d t) [0 .. t]) . reader
  where
    beats d t x = (t - x) * x > d
    reader = either (error "Bad parse") id . parseOnly (zip <$> time <*> ("\n" *> distance) <* endOfInput)
    time = "Time:" *> skipSpace *> parser
    distance = "Distance:" *> skipSpace *> parser

part1 :: Text -> Int
part1 = solve (decimal `sepBy1'` skipSpace)

part2 :: Text -> Int
part2 = solve ((: []) . read . filter (/= ' ') . T.unpack <$> takeTill isEndOfLine)

main :: IO ()
main = do
    input <- T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
