module Main where

import Data.Bits (shiftL, shiftR, xor)

parse_ :: String -> [Int]
parse_ = fmap read . lines

next :: Int -> Int
next a = d
  where
    m = 16_777_216
    b = (a `xor` (a `shiftL` 6)) `mod` m
    c = (b `xor` (b `shiftR` 5)) `mod` m
    d = (c `xor` (c `shiftL` 11)) `mod` m

part1 :: [Int] -> Int
part1 = sum . fmap ((!! 2000) . iterate next)

main :: IO ()
main = do
    input <- parse_ <$> readFile "input.txt"
    print $ part1 input
