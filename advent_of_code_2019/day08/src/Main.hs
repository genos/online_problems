{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- with help from https://github.com/mstksg/advent-of-code-2019/blob/master/reflections.md#day-8

module Main where

import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Linear.V2 (V2 (..))

width, dim :: Int
width = 25
dim = width * 6

part1 :: Text -> Int
part1 =
  product
    . sum
    . fmap (\case '1' -> V2 1 0; '2' -> V2 0 1; _ -> V2 0 0)
    . T.unpack
    . minimumBy (comparing $ T.count "0")
    . T.chunksOf dim

part2 :: Text -> Text
part2 =
  T.pack
    . fmap (T.head . T.dropWhile (== '2'))
    . T.transpose
    . T.chunksOf dim

image :: Text -> Text
image = T.unlines . T.chunksOf width . T.map (\case '0' -> ' '; _ -> '#')

main :: IO ()
main = do
  input <- T.readFile "input"
  print $ part1 input
  T.putStrLn . image $ part2 input
