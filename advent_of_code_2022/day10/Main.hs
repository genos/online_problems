{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Bool (bool)
import Data.Foldable (foldl', traverse_)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T

readAndInterpret :: Text -> [Int -> Int]
readAndInterpret = either (error "Bad parse") concat . parseOnly (f `sepBy1'` endOfLine)
  where
    f = choice [[id] <$ string "noop", (\n -> [id, (+ n)]) <$> (string "addx " *> signed decimal)]

solve :: (x -> Text) -> (x -> (Int -> Int) -> x) -> x -> [Int -> Int] -> Text
solve g f z = g . foldl' f z

part1 :: [Int -> Int] -> Text
part1 = solve (pack . show . \(_, _, s) -> s) f (1, 1, 0)
  where
    f (i, x, s) g = (succ i, g x, s + i * x * bool 0 1 (i `elem` [20, 60, 100, 140, 180, 220]))

main :: IO ()
main = do
    input <- readAndInterpret <$> T.readFile "input.txt"
    traverse_ (T.putStrLn . ($ input)) [part1]
