{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text qualified as A
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map.Strict qualified as M
import Data.Text (Text)
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
    go :: Int -> Text -> [Dir] -> Int
    go n _ [] = n -- impossible by design, whatevs
    go n "ZZZ" _ = n
    go n p (d : ds) =
        let (l, r) = network M.! p
            p' = if d == L then l else r
            n' = succ n
         in go n' p' ds

main :: IO ()
main = do
    traverse_ (print . part1 . readMap) [test1, test2]
    input <- readMap <$> T.readFile "input.txt"
    print $ part1 input
