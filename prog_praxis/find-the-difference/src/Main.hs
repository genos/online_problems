module Main where

import           Data.Bits     (xor)
import           Data.Char     (chr, ord)
import           Data.Foldable (foldl')
import qualified Data.Set      as S

data Solution a b = S { _in      :: String -> a
                      , _compute :: a -> a -> b
                      , _out     :: b -> Char
                      }

run :: Solution a b -> String -> String -> Char
run (S i c o) x y = o $ c (i x) (i y)

s1 :: Solution (S.Set Char) (S.Set Char)
s1 =
  S S.fromList (\x y -> (x `S.union` y) S.\\ (x `S.intersection` y)) S.findMin

s2 :: Solution Int Int
s2 = S (sum . fmap ord) (-) (chr . abs)

s3 :: Solution Int Int
s3 = S (foldl' xor 0 . fmap ord) xor chr

main :: IO ()
main = do
  let x = "abcdef"
      y = "bfxdeac"
  print $ all (== 'x') [run s1 x y, run s2 x y, run s3 x y]
