module Main (main) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.IO qualified as T

parse_ :: Text -> [Int]
parse_ = either (error "Bad parse") id . parseOnly (line `sepBy1'` char '\n')
  where
    line = ((char 'L' $> negate) <|> (char 'R' $> id)) <*> decimal

part1 :: [Int] -> Int
part1 = fst . foldl' f (0, 50)
  where
    f (n, s) i = let r = (s + i) `mod` 100 in (n + bool 0 1 (r == 0), r)

part2 :: [Int] -> Int
part2 = fst . foldl' f_ (0, 50)

f_ :: (Int, Int) -> Int -> (Int, Int)
f_ (n, s) = step (n, s)
  where
    step (a, b) c
        | c == 0 = (a, b)
        | otherwise =
            let
                a' = a + bool 0 1 (b' == 0)
                b' = (b + g) `mod` 100
                c' = c - g
                g = signum c
             in
                step (a', b') c'

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
