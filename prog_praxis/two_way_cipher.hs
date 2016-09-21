module Main where
import Data.Char (chr, ord)
sub x y = chr $ 65 + (x' - y') `mod` 26
  where
    x' = ord x - 65
    y' = ord y - 65
crypt = zipWith sub
cipher = "ABCDEFGHIJK"
key1 = crypt cipher "PROGRAMMING"
key2 = crypt cipher "PRAXISXXXXX"
main = do
  print cipher
  mapM_ (print . crypt cipher) [key1, key2]
