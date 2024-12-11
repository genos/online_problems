module Main where

import Data.Foldable (traverse_)
import Data.Word (Word64)

parse :: String -> [Word64]
parse = fmap read . words

part1 :: [Word64] -> Int
part1 = undefined

part2 :: [Word64] -> Int
part2 = undefined

main :: IO ()
main = do
    m <- parse <$> readFile "test.txt"
    traverse_ (print . ($ m)) [part1, part2]
