{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Foldable        (traverse_)
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Data.Vector.Unboxed  (Vector)
import qualified Data.Vector.Unboxed  as V

type Crabs = Vector Int
type FuelConsumption = (Int -> Int -> Int)

readCrabs :: Text -> Crabs
readCrabs = either (error "Bad parse") V.fromList <$> parseOnly (decimal `sepBy1'` ",")

solve :: Crabs -> FuelConsumption -> Int
solve crabs fuel = V.minimum . V.map total $ V.enumFromN lo (hi - lo)
 where
  lo = V.minimum crabs
  hi = V.maximum crabs
  total n = V.sum $ V.map (fuel n) crabs

part1 :: FuelConsumption
part1 n = abs . (n -)

part2 :: FuelConsumption
part2 n = V.sum . V.enumFromN 0 . (succ . abs . (n -))

main :: IO ()
main = do
  crabs <- readCrabs <$> T.readFile "input.txt"
  traverse_ (print . solve crabs) [part1, part2]
