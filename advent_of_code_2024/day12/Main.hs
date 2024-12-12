module Main where

import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Linear.V2 (V2 (..))

type Coord = V2 Int
type Map = M.Map Coord Char
data Region = R {_plant :: Char, _plots :: S.Set Coord} deriving (Eq, Ord, Show)
type Garden = S.Set Region

parse :: String -> Garden
parse input = grow (M.keysSet m) S.empty
  where
    m = M.fromList [(V2 i j, c) | (j, l) <- zip [0 ..] $ reverse $ lines input, (i, c) <- zip [0 ..] l]
    grow cs g
        | S.null cs = g
        | otherwise = grow cs'' (S.insert r g)
      where
        (c, cs') = S.deleteFindMin cs
        r = plant c
        cs'' = cs' S.\\ _plots r
    plant c = go (S.singleton c) S.empty
      where
        pl = m M.! c
        go todo done
            | S.null todo = R pl done
            | otherwise = go (xs `S.union` ss) (S.insert x done)
          where
            (x, xs) = S.deleteFindMin todo
            same = S.filter ((== Just pl) . (m M.!?)) . S.fromList $ nbd x
            ss = same S.\\ done

nbd :: Coord -> [Coord]
nbd x = [x + u | u <- [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]]

area, perimeter, sides :: Region -> Int
area = S.size . _plots
perimeter (R _ ps) = S.foldl' (\acc c -> acc + f c) 0 ps
  where
    f c = 4 - sum [1 | n <- nbd c, n `S.member` ps]
sides (R _ ps) = S.foldl' (\acc c -> acc + sum (corners <$> xNbd c)) 0 ps
  where
    corners (x, y, z) = b2i convex + b2i concave
      where
        ins@(x', y', _) = (x `S.member` ps, y `S.member` ps, z `S.member` ps)
        convex = ins == (True, True, False)
        concave = (x', y') == (False, False)
    xNbd c =
        [ (c + x, c + y, c + z)
        | (x, y, z) <-
            [ (V2 (-1) 0, V2 0 1, V2 (-1) 1)
            , (V2 1 0, V2 0 1, V2 1 1)
            , (V2 1 0, V2 0 (-1), V2 1 (-1))
            , (V2 (-1) 0, V2 0 (-1), V2 (-1) (-1))
            ]
        ]
    b2i b = if b then 1 else 0

solve :: (Region -> Int) -> Garden -> Int
solve f = S.foldl' (\p r -> p + area r * f r) 0

part1, part2 :: Garden -> Int
part1 = solve perimeter
part2 = solve sides

main :: IO ()
main = do
    g <- parse <$> readFile "input.txt"
    traverse_ (print . ($ g)) [part1, part2]
