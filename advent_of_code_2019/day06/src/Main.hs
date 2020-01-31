{-# LANGUAGE OverloadedStrings #-}

module Main where

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
import Data.Bifunctor (second)
import Data.Foldable (traverse_)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

readGraph :: Text -> AdjacencyMap Text
readGraph = overlays . fmap readEdge . T.lines
  where
    readEdge = uncurry edge . second T.tail . T.breakOn ")"

part1 :: AdjacencyMap Text -> Int
part1 g = sum (fmap numOrbits v) - length v
  where
    numOrbits = length . (`dfs` g) . (: [])
    v = vertexList g

part2 :: AdjacencyMap Text -> Int
part2 g = S.size you' + S.size san'
  where
    g' = transpose g
    you = S.fromList $ reachable "YOU" g'
    san = S.fromList $ reachable "SAN" g'
    you' = you S.\\ san
    san' = san S.\\ you

main :: IO ()
main = do
  input <- readGraph <$> T.readFile "input"
  traverse_ (print . ($ input)) [part1, part2]
