{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Data.Vector.Unboxed  (Vector)
import qualified Data.Vector.Unboxed  as V

readCrabs :: Text -> Vector Int
readCrabs = either (error "Parse") V.fromList <$> parseOnly (decimal `sepBy1'` ",")

fuelRequired :: Vector Int -> Int -> Int
fuelRequired crabs i = V.sum $ V.map (abs . (i -)) crabs

part1 :: Vector Int -> Int
part1 crabs = V.minimum . V.map (fuelRequired crabs) $ V.enumFromTo (V.minimum crabs) (V.maximum crabs)

main :: IO ()
main = do
  crabs <- readCrabs <$> T.readFile "input.txt"
  print $ part1 crabs
