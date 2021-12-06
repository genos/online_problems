{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Foldable        (foldl', traverse_)
import           Data.Sequence        (Seq, empty, fromList, (><), (|>))
import           Data.Text            (Text)
import qualified Data.Text.IO         as T

readFish :: Text -> Seq Word
readFish = either (const empty) fromList . parseOnly (decimal `sepBy1'` ",")

lanternFish :: Int -> Seq Word -> Int
lanternFish n = length . (!! n) . iterate step
 where
  step = uncurry (><) . foldl' tic (empty, empty)
  tic (!l, !r) 0 = (l |> 6, r |> 8)
  tic (!l, !r) !k = (l |> k - 1, r)

part1 :: Seq Word -> Int
part1 = lanternFish 80

part2 :: Seq Word -> Int
part2 = lanternFish 256

main :: IO ()
main = do
  fish <- readFish <$> T.readFile "test.txt"
  traverse_ (print . ($ fish)) [part1, part2]
