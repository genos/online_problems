{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text qualified as A
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

test1 :: Text
test1 =
    "RL\n\
    \\n\
    \AAA = (BBB, CCC)\n\
    \BBB = (DDD, EEE)\n\
    \CCC = (ZZZ, GGG)\n\
    \DDD = (DDD, DDD)\n\
    \EEE = (EEE, EEE)\n\
    \GGG = (GGG, GGG)\n\
    \ZZZ = (ZZZ, ZZZ)"

test2 :: Text
test2 =
    "LLR\n\
    \\n\
    \AAA = (BBB, BBB)\n\
    \BBB = (AAA, ZZZ)\n\
    \ZZZ = (ZZZ, ZZZ)"

data Dir = L | R deriving (Eq, Ord, Show)
type Map = ([Dir], M.Map Text (Text, Text))

readMap :: Text -> Map
readMap = either (error "Bad parse") id . A.parseOnly (((,) <$> dirs <*> ("\n\n" *> network)) <* A.endOfInput)
  where
    dirs = A.many1' $ A.choice ["L" $> L, "R" $> R]
    network = M.fromList <$> row `A.sepBy1'` "\n"
    row = (\x y z -> (x, (y, z))) <$> A.take 3 <*> (" = (" *> A.take 3) <*> (", " *> A.take 3 <* ")")

part1 :: Map -> Int
part1 (dirs, network) = go 0 "AAA" $ cycle dirs
  where
    go n _ [] = n -- impossible by design, whatevs
    go n "ZZZ" _ = n
    go n p (d : ds) =
        let (l, r) = network M.! p
            p' = if d == L then l else r
         in go (n + 1) p' ds

part2 :: Map -> Int
part2 (dirs, network) = go 0 (M.keys $ M.filterWithKey (\k _ -> T.last k == 'A') network) $ cycle dirs
  where
    go n _ [] = n -- impossible by design, whatevs
    go n ps _ | all ((== 'Z') . T.last) ps = n
    go n ps (d : ds) =
        let f = if d == L then fst else snd
            ps' = fmap (f . (network M.!)) ps
         in go (n + 1) ps' ds

main :: IO ()
main = do
    input <- readMap <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
