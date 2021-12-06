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
type Diagram = Map Coordinate Int
type Pair = (V2 Int, V2 Int)

pairsFrom :: Text -> [Pair]
pairsFrom = fromRight [] . parseOnly (pair `sepBy1'` "\n")
 where
  pair  = (,) <$> coord <*> (" -> " *> coord)
  coord = V2 <$> decimal <*> ("," *> decimal)

largest :: Coordinate -> Pair -> Coordinate
largest (V2 a b) (V2 x y, V2 z w) = V2 (maximum [a, x, z]) (maximum [b, y, w])

addOr1 :: Diagram -> Coordinate -> Diagram
addOr1 d k = M.insertWith (+) k 1 d

lines1 :: Diagram -> Pair -> Diagram
lines1 d (V2 x y, V2 z w)
  | x == z    = foldl' addOr1 d [ V2 x y' | y' <- [min y w .. max y w] ]
  | y == w    = foldl' addOr1 d [ V2 x' y | x' <- [min x z .. max x z] ]
  | otherwise = d

gt1 :: (Coordinate, Diagram) -> Int
gt1 (V2 xL yL, d) =
  length [ V2 a b | a <- [0 .. xL], b <- [0 .. yL], M.findWithDefault 0 (V2 a b) d > 1 ]

part1 :: [Pair] -> Int
part1 = gt1 . foldl' (\(v, d) -> largest v &&& lines1 d) (zero, M.empty)

lines2 :: Diagram -> Pair -> Diagram
lines2 = error "TODO"

part2 :: [Pair] -> Int
part2 = gt1 . foldl' (\(v, d) -> largest v &&& lines2 d) (zero, M.empty)

main :: IO ()
main = do
  input <- pairsFrom <$> T.readFile "test.txt"
  traverse_ (print . ($ input)) [part1, part2]
