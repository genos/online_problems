{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow               ((&&&))
import           Data.Attoparsec.Text
import           Data.Foldable               (for_, traverse_)
import           Data.List                   (group, sort)
import           Data.Text                   (Text)
import qualified Data.Text.IO                as T
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

readFish :: Text -> Vector Int
readFish = either (const blank) ((blank V.//) . tally) . numbers
 where
  numbers = parseOnly (decimal `sepBy1'` ",")
  blank   = V.replicate 9 0
  tally   = fmap (head &&& length) . group . sort

lanternFish :: Int -> Vector Int -> Int
lanternFish n = V.sum . (!! n) . iterate step
 where
  step old = V.create $ do
    new <- M.unsafeNew 9
    for_ [1 .. 8] $ \i -> do
      x <- V.unsafeIndexM old i
      M.write new (i - 1) x
    a <- V.unsafeIndexM old 0
    b <- V.unsafeIndexM old 7
    M.write new 6 (a + b)
    M.write new 8 a
    pure new

part1 :: Vector Int -> Int
part1 = lanternFish 80

part2 :: Vector Int -> Int
part2 = lanternFish 256

main :: IO ()
main = do
  fish <- readFish <$> T.readFile "input.txt"
  traverse_ (print . ($ fish)) [part1, part2]
