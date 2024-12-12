module Main where

import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Linear.V2 (V2 (..))

type Coord = V2 Int
type Map = M.Map Coord Char
data Region = Region
    { _plant :: {-# UNPACK #-} !Char
    , _plots :: S.Set Coord
    , _area :: {-# UNPACK #-} !Int
    , _perimeter :: {-# UNPACK #-} !Int
    , _sides :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord, Show)
type Garden = S.Set Region

parse :: String -> Map
parse input =
    M.fromList
        [ (V2 i j, c)
        | (j, l) <- zip [0 ..] $ reverse $ lines input
        , (i, c) <- zip [0 ..] l
        ]

plant :: Map -> Coord -> Region
plant m c = go (S.singleton c) S.empty 0 0 0
  where
    pl = m M.! c
    go xs cs a p s
        | S.null xs = Region pl cs a p s
        | otherwise = go (ys `S.union` ss) (S.insert x cs) (a + 1) (p + S.size ds) s'
      where
        (x, ys) = S.deleteFindMin xs
        nbd = S.fromList [x + u | u <- [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]]
        (same, diff) = S.partition ((== Just pl) . (m M.!?)) nbd
        ss = same S.\\ cs
        ds = diff S.\\ cs
        s' = s + S.size ds - S.size ss

grow :: Map -> Garden
grow m = go (M.keysSet m) S.empty
  where
    go cs g
        | S.null cs = g
        | otherwise = let (c, cs') = S.deleteFindMin cs; r = plant m c in go (cs' S.\\ _plots r) (S.insert r g)

solve :: (Region -> Int) -> Garden -> Int
solve f = S.foldl' (\p r -> p + _area r * f r) 0

part1, part2 :: Garden -> Int
part1 = solve _perimeter
part2 = solve _sides

main :: IO ()
main = do
    g <- grow . parse <$> readFile "test.txt"
    traverse_ (print . ($ g)) [part1, part2]
