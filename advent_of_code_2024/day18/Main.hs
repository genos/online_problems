{-# LANGUAGE OverloadedStrings #-}

module Main where

import Algorithm.Search (aStar)
import Data.Attoparsec.Text hiding (take)
import Data.Foldable (find)
import Data.List (tails)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.IO qualified as T
import Linear.V2

type Coord = V2 Int
lo, hi :: Int
lo = 0
hi = 70

parse_ :: Text -> [Coord]
parse_ = either (error "Bad parse") id . parseOnly cs
  where
    cs = c `sepBy1'` "\n"
    c = V2 <$> decimal <*> ("," *> decimal)

dist :: Coord -> Coord -> Int
dist c d = sum . abs $ c - d

neighbors :: S.Set Coord -> (Coord -> [Coord])
neighbors bs (V2 x y) =
    [ c
    | c <- [V2 (x - 1) y, V2 (x + 1) y, V2 x (y - 1), V2 x (y + 1)]
    , minimum c >= lo && maximum c <= hi && not (c `S.member` bs)
    ]

search :: [Coord] -> Maybe (Int, [Coord])
search cs = aStar (neighbors bs) dist (`dist` end) (== end) start
  where
    bs = S.fromList cs
    start = pure lo
    end = pure hi

part1 :: [Coord] -> Maybe Int
part1 = fmap fst . search . take 1024

part2 :: [Coord] -> Maybe Coord
part2 = fmap last . find (null . search) . reverse . tails

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    print $ part1 input
    print $ part2 input
