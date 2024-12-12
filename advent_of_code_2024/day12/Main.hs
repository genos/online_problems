module Main where

import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Linear.V2 (V2 (..))

type Coord = V2 Int
type Map = M.Map Coord Char
data Region = Region {_plant :: Char, _plots :: S.Set Coord} deriving (Eq, Ord, Show)
type Garden = S.Set Region

parse :: String -> Map
parse input =
    M.fromList
        [ (V2 i j, c)
        | (j, l) <- zip [0 ..] $ reverse $ lines input
        , (i, c) <- zip [0 ..] l
        ]

nbd :: Coord -> [Coord]
nbd c = [c + u | u <- [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]]

area, perimeter, price :: Region -> Int
area = S.size . _plots
perimeter r = S.foldl' (\acc c -> acc + f c) 0 ps
  where
    f c = 4 - sum [1 | n <- nbd c, n `S.member` ps]
    ps = _plots r
price = (*) <$> area <*> perimeter

plant :: Map -> Coord -> Region
plant m c = go [c] S.empty
  where
    p = m M.! c
    go [] cs = Region p cs
    go (x : xs) cs = go ([y | y <- nbd x, not $ y `S.member` cs, m M.!? y == Just p] <> xs) (S.insert x cs)

grow :: Map -> Garden
grow m = go (M.keysSet m) S.empty
  where
    go cs g
        | S.null cs = g
        | otherwise = let (c, cs') = S.deleteFindMin cs; r = plant m c in go cs' (S.insert r g)

part1 :: Garden -> Int
part1 = S.foldl' (\p r -> p + price r) 0

main :: IO ()
main = do
    g <- grow . parse <$> readFile "input.txt"
    traverse_ (print . ($ g)) [part1]
