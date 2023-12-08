{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text qualified as A
import Data.Foldable (foldl', traverse_)
import Data.Functor (($>))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

type Node = Text
type Dir = ((Node, Node) -> Node)
type Map = ([Dir], M.Map Node (Node, Node))

readMap :: Text -> Map
readMap = either (error "Bad parse") id . A.parseOnly (((,) <$> dirs <*> ("\n\n" *> network)) <* A.endOfInput)
  where
    dirs = A.many1' $ A.choice ["L" $> fst, "R" $> snd]
    network = M.fromList <$> row `A.sepBy1'` "\n"
    row = (\x y z -> (x, (y, z))) <$> A.take 3 <*> (" = (" *> A.take 3) <*> (", " *> A.take 3 <* ")")

dist :: Map -> (Node -> Bool) -> Node -> Word
dist (dirs, network) done = go 0 (cycle dirs)
  where
    go n [] _ = n -- impossible by design, whatevs
    go n (d : ds) p
        | done p = n
        | otherwise = go (n + 1) ds (d $ network M.! p)

part1 :: Map -> Word
part1 m = dist m (== "ZZZ") "AAA"

part2 :: Map -> Word
part2 m@(_, network) =
    foldl' lcm 1 $ dist m ((== 'Z') . T.last) <$> M.keys (M.filterWithKey (\k _ -> T.last k == 'A') network)

main :: IO ()
main = do
    input <- readMap <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
