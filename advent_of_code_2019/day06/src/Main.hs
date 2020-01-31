{-# LANGUAGE OverloadedStrings #-}
module Main where

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
import Data.Bifunctor (second)
import Data.Foldable (traverse_)
import Data.List (intersect)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

readGraph :: Text -> AdjacencyMap Text
readGraph = overlays . fmap readEdge . T.lines
  where readEdge = uncurry edge . second T.tail  . T.breakOn ")"

part1 :: AdjacencyMap Text -> Int
part1 g = sum (fmap numOrbits v) - length v
  where
    numOrbits = length . (`dfs` g) . (:[])
    v = vertexList g

part2 :: AdjacencyMap Text -> Int
part2 g = len x + len y - 2
  where
    g' = transpose g
    x = reachable "YOU" g'
    y = reachable "SAN" g'
    meet = head $ intersect x y
    len = length . takeWhile (/= meet)

main :: IO ()
main = do
  input <- readGraph <$> T.readFile "input"
  traverse_ (print . ($ input)) [part1, part2]
