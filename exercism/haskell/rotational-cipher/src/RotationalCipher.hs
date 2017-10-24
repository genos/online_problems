module RotationalCipher (rotate) where

import Data.Char (chr, isAsciiLower, isAsciiUpper, ord)

rotate :: Int -> String -> String
rotate = fmap . r
 where
  r :: Int -> Char -> Char
  r n c | isAsciiLower c = f n 'a' c
        | isAsciiUpper c = f n 'A' c
        | otherwise      = c
  f :: Int -> Char -> Char -> Char
  f n a b = let a' = ord a in chr $ a' + (ord b - a' + n) `mod` 26
