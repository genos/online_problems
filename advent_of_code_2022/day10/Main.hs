{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text.IO as T

readAndInterpret :: Text -> [Int -> Int]
readAndInterpret = either (error "Bad parse") concat . parseOnly (f `sepBy1'` endOfLine)
  where
    f = choice [[id] <$ string "noop", (\n -> [id, (+ n)]) <$> (string "addx " *> signed decimal)]

part1 :: [Int -> Int] -> Int
part1 = snd . foldl' f (1, 0) . zip [1 ..]
  where
    f (x, s) (i, g) = (x', s')
      where
        x' = g x
        s' = s + i * x * bool 0 1 (i `elem` [20, 60, 100, 140, 180, 220])

main :: IO ()
main = do
    input <- readAndInterpret <$> T.readFile "input.txt"
    print $ part1 input
