{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as T

test :: Text
test =
    "seeds: 79 14 55 13\n\
    \\n\
    \seed-to-soil map:\n\
    \50 98 2\n\
    \52 50 48\n\
    \\n\
    \soil-to-fertilizer map:\n\
    \0 15 37\n\
    \37 52 2\n\
    \39 0 15\n\
    \\n\
    \fertilizer-to-water map:\n\
    \49 53 8\n\
    \0 11 42\n\
    \42 0 7\n\
    \57 7 4\n\
    \\n\
    \water-to-light map:\n\
    \88 18 7\n\
    \18 25 70\n\
    \\n\
    \light-to-temperature map:\n\
    \45 77 23\n\
    \81 45 19\n\
    \68 64 13\n\
    \\n\
    \temperature-to-humidity map:\n\
    \0 69 1\n\
    \1 0 69\n\
    \\n\
    \humidity-to-location map:\n\
    \60 56 37\n\
    \56 93 4"

data Row = Row {_output :: Word, _input :: Word, _num :: Word} deriving (Show)
type Map = [Row]

mapLup :: Word -> Map -> Word
mapLup w m = if null answers then w else head answers
  where
    answers = mapMaybe lup m
    lup (Row o i n)
        | i <= w && w <= i + n = Just $ o + w - i
        | otherwise = Nothing

data Almanac = Almanac {_seeds :: [Word], _maps :: [Map]} deriving (Show)

readAlmanac :: Text -> Almanac
readAlmanac = either (error "Bad parse") id . parseOnly (almanac <* endOfInput)
  where
    almanac = Almanac <$> ("seeds: " *> seeds <* "\n\nseed-to-soil map:\n") <*> (imap `sepBy1'` ("\n\n" *> name <* "\n"))
    seeds = decimal `sepBy1'` " "
    name = many1' letter *> "-" *> many1' letter *> "-" *> many1' letter *> " map:"
    imap = row `sepBy1'` "\n"
    row = Row <$> decimal <*> (" " *> decimal) <*> (" " *> decimal)

part1 :: Almanac -> Word
part1 (Almanac seeds maps) = minimum $ fmap (\s -> foldl' mapLup s maps) seeds

main :: IO ()
main = do
    print . part1 $ readAlmanac test
    input <- readAlmanac <$> T.readFile "input.txt"
    print $ part1 input
