{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text qualified as A
import Data.Foldable (foldl', traverse_)
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

distance :: Map -> (Text -> Bool) -> Text -> Word
distance (dirs, network) f pos = go 0 pos $ cycle dirs
  where
    go !n _ [] = n -- impossible by design, whatevs
    go !n !p (!d : ds)
        | f p = n
        | otherwise =
            let (!l, !r) = network M.! p
                p' = if d == L then l else r
             in go (n + 1) p' ds

part1 :: Map -> Word
part1 m = distance m (== "ZZZ") "AAA"

part2 :: Map -> Word
part2 m@(_, network) = foldl' lcm 1 $ distance m ((== 'Z') . T.last) <$> ps
  where
    ps = M.keys $ M.filterWithKey (\k _ -> T.last k == 'A') network

main :: IO ()
main = do
    input <- readMap <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
