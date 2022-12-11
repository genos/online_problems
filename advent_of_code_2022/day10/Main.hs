{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Bool (bool)
import Data.Foldable (foldl', traverse_)
import Data.Function ((&))
import Data.List (scanl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

readInterpretApply :: Text -> [Int]
readInterpretApply = either (error "Bad parse") (scanl' (&) 1 . concat) . parseOnly (f `sepBy1'` endOfLine)
  where
    f = choice [[id] <$ string "noop", (\n -> [id, (+ n)]) <$> (string "addx " *> signed decimal)]

solve :: ((a, b) -> Int -> (a, b)) -> (a, b) -> (b -> Text) -> [Int] -> Text
solve f z g = g . snd . foldl' f z

part1 :: [Int] -> Text
part1 = solve f (1, 0) (T.pack . show)
  where
    f (i, s) n = (succ i, s + i * n * bool 0 1 (i `elem` [20, 60, 100, 140, 180, 220]))

part2 :: [Int] -> Text
part2 = solve f (0, T.empty) (T.unlines . T.chunksOf 40 . T.take 240 . T.reverse)
  where
    f (i, screen) sprite = (succ i `mod` 40, T.cons c screen)
      where
        c = bool ' ' '#' (abs (i - sprite) <= 1)

main :: IO ()
main = do
    sprites <- readInterpretApply <$> T.readFile "input.txt"
    traverse_ (T.putStrLn . ($ sprites)) [part1, part2]
