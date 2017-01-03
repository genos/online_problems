{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Protolude
import Data.FileEmbed (embedStringFile)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

input :: Text
input = T.pack $(embedStringFile "input.txt")

test1Input :: Text
test1Input = T.pack $(embedStringFile "test1input.txt")

test2Input :: Text
test2Input = T.pack $(embedStringFile "test2input.txt")

type Tile = Int
type Row = U.Vector Tile

textToRow :: Text -> Row
textToRow = U.concatMap f . U.fromList . T.unpack where
  f :: Char -> Row
  f '.' = U.singleton 1
  f '^' = U.singleton 0
  f _ = U.empty
  {-# INLINE f #-}

nextTile :: Tile -> Tile -> Tile -> Tile
nextTile 0 0 1 = 0
nextTile 1 0 0 = 0
nextTile 0 1 1 = 0
nextTile 1 1 0 = 0
nextTile _ _ _ = 1

nextRow :: Row -> Row
nextRow r = U.zipWith3 nextTile r' (U.tail r') (U.tail $ U.tail r') where
  r' = 1 `U.cons` r `U.snoc` 1

test1 :: Bool
test1 = computed == expected where
  expected = V.map textToRow . V.fromList . T.lines $ test1Input
  computed = V.iterateN 3 nextRow $ V.head expected

test2 :: Bool
test2 = computed == expected where
  expected = V.map textToRow . V.fromList . T.lines $ test2Input
  computed = V.iterateN 10 nextRow $ V.head expected

countSafe :: Vector Row -> Int
countSafe = V.sum . V.map U.sum

test3 :: Bool
test3 = computed == expected where
  expected = 38
  computed = countSafe . V.map textToRow . V.fromList . T.lines $ test2Input

part1 :: Int
part1 = countSafe . V.iterateN 40 nextRow $ textToRow input

part2 :: Int
part2 = countSafe . V.iterateN 400000 nextRow $ textToRow input

main :: IO ()
main = do
  print $ all identity [test1, test2, test3]
  mapM_ print [part1, part2]
