{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Foldable        (traverse_)
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Data.Vector.Unboxed  (Vector)
import qualified Data.Vector.Unboxed  as V

readCrabs :: Text -> Vector Int
readCrabs = either (error "Parse") V.fromList <$> parseOnly (decimal `sepBy1'` ",")

solve :: Vector Int -> (Vector Int -> Int -> Int) -> Int
solve crabs fuel =
  V.minimum . V.map (fuel crabs) $ V.enumFromTo (V.minimum crabs) (V.maximum crabs)

fuelPart1 :: Vector Int -> Int -> Int
fuelPart1 crabs i = V.sum $ V.map (abs . (i -)) crabs

fuelPart2 :: Vector Int -> Int -> Int
fuelPart2 crabs i = V.sum $ V.map (V.sum . V.enumFromN 0 . (succ . abs . (i -))) crabs

main :: IO ()
main = do
  crabs <- readCrabs <$> T.readFile "input.txt"
  traverse_ (print . solve crabs) [fuelPart1, fuelPart2]
