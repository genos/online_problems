{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Foldable (foldl', traverse_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as T

type Row = (Word, Word, Word)
type Map = [Row]
type Almanac = ([Word], [Map])

readAlmanac :: Text -> Almanac
readAlmanac = either (error "Bad parse") id . parseOnly (almanac <* endOfInput)
  where
    almanac =
        (,)
            <$> ("seeds: " *> seeds <* "\n\nseed-to-soil map:\n")
            <*> (imap `sepBy1'` ("\n\n" *> takeTill isSpace <* " map:\n"))
    seeds = decimal `sepBy1'` " "
    imap = row `sepBy1'` "\n"
    row = (,,) <$> decimal <*> (" " *> decimal) <*> (" " *> decimal)

lup :: Word -> Map -> Word
lup w m = case answers of
    [] -> w
    (a : _) -> a
  where
    answers = mapMaybe f m
    f (o, i, n)
        | i <= w && w <= i + n = Just $ o + w - i
        | otherwise = Nothing

solve :: ([Word] -> [Word]) -> Almanac -> Word
solve f (seeds, maps) = minimum $ (\s -> foldl' lup s maps) <$> f seeds

part1 :: Almanac -> Word
part1 = solve id

part2 :: Almanac -> Word
part2 = solve f
  where
    f [] = []
    f [_] = error "Shouldn't happen"
    f (x : y : zs) = [x .. x + y - 1] <> f zs

main :: IO ()
main = do
    input <- readAlmanac <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
