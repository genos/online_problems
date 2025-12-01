module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.IO qualified as T

parse_ :: Text -> [Int]
parse_ = either (error "Bad parse") id . parseOnly (line `sepBy1'` char '\n')
  where
    line = ((char 'L' $> negate) <|> (char 'R' $> id)) <*> decimal

part1 :: [Int] -> Int
part1 = length . filter (== 0) . scanl (\a b -> (a + b) `mod` 100) 50

part2 :: [Int] -> Int
part2 = fst . foldl' f (0, 50)
  where
    f (n, p) i
        | i == 0 = (n, p)
        | otherwise =
            let
                n' = n + if p' == 0 then 1 else 0
                p' = (p + s) `mod` 100
                s = signum i
             in
                f (n', p') (i - s)

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
