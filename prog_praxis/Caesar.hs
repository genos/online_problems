module Main where

import Data.Char (chr, isAsciiUpper, ord)
import Data.Foldable (traverse_)

shift :: Int -> Char -> Char
shift n c | isAsciiUpper c = chr $ a + ((n + (ord c - a) `mod` 26))
          | otherwise      = c
    where
        a = ord 'A'

caesar :: Int -> String -> String
caesar n = map $ shift n

main :: IO ()
main = traverse_ putStrLn [p, c, p']
    where
        p  = "PROGRAMMING PRAXIS"
        c  = caesar 3 p
        p' = caesar (-3) c
