module Main where

import Data.Foldable (traverse_)
import Data.Word (Word64)

parse :: String -> [Word64]
parse = fmap read . words

digits :: Word64 -> [Word64]
digits = go []
  where
    go ds n =
        let (q, r) = n `divMod` 10
            ds' = r : ds
         in if q == 0 then ds' else go ds' q

undigits :: [Word64] -> Word64
undigits = foldl' (\acc d -> 10 * acc + d) 0

blink :: Word64 -> [Word64]
blink 0 = [1]
blink n | even d = [undigits l, undigits r]
  where
    ds = digits n
    d = length ds
    (l, r) = splitAt (d `div` 2) ds
blink n = [2024 * n]

part1 :: [Word64] -> Int
part1 = length . (!! 25) . iterate (concatMap blink)

part2 :: [Word64] -> Int
part2 = length . (!! 75) . iterate (concatMap blink)

main :: IO ()
main = do
    m <- parse <$> readFile "input.txt"
    traverse_ (print . ($ m)) [part1, part2]
