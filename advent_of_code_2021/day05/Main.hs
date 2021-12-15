{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow        ((&&&))
import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl', traverse_)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Linear.V2
import           Linear.Vector

type Coordinate = V2 Int
type Pair = (Coordinate, Coordinate)
type Diagram = Map Coordinate Int

pairsFrom :: Text -> [Pair]
pairsFrom = fromRight (error "Bad parse") . parseOnly (pair `sepBy1'` "\n")
 where
  pair  = (,) <$> coord <*> (" -> " *> coord)
  coord = V2 <$> decimal <*> ("," *> decimal)

largest :: Coordinate -> Pair -> Coordinate
largest (V2 a b) (V2 x y, V2 z w) = V2 (maximum [a, x, z]) (maximum [b, y, w])

addOr1 :: Diagram -> Coordinate -> Diagram
addOr1 d k = M.insertWith (+) k 1 d

lines1 :: Diagram -> Pair -> Diagram
lines1 d (V2 x1 y1, V2 x2 y2) = foldl' addOr1 d vs
 where
  (xs, ys) = ([min x1 x2 .. max x1 x2], [min y1 y2 .. max y1 y2])
  vs | x1 == x2  = V2 x1 <$> ys
     | y1 == y2  = (`V2` y1) <$> xs
     | otherwise = []

gt1 :: (Coordinate, Diagram) -> Int
gt1 (V2 xL yL, d) =
  length [ V2 x y | x <- [0 .. xL], y <- [0 .. yL], M.findWithDefault 0 (V2 x y) d > 1 ]

part1 :: [Pair] -> Int
part1 = gt1 . foldl' (\(v, d) -> largest v &&& lines1 d) (zero, M.empty)

lines2 :: Diagram -> Pair -> Diagram
lines2 d (V2 x1 y1, V2 x2 y2) = foldl' addOr1 d vs
 where
  (xs  , ys  ) = ([min x1 x2 .. max x1 x2], [min y1 y2 .. max y1 y2])
  (xSig, ySig) = (signum $ x2 - x1, signum $ y2 - y1)
  xD           = abs $ x1 - x2
  vs
    | x1 == x2                       = V2 x1 <$> ys
    | y1 == y2                       = (`V2` y1) <$> xs
    | abs (x1 - x2) == abs (y1 - y2) = [ V2 x1 y1 + i *^ V2 xSig ySig | i <- [0 .. xD] ]
    | otherwise                      = []

part2 :: [Pair] -> Int
part2 = gt1 . foldl' (\(v, d) -> largest v &&& lines2 d) (zero, M.empty)

main :: IO ()
main = do
  input <- pairsFrom <$> T.readFile "input.txt"
  traverse_ (print . ($ input)) [part1, part2]
