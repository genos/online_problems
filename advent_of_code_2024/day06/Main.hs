module Main where

import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Linear.V2 (V2 (..))

type Coord = V2 Int
type Map = M.Map Coord Char
data Dir = U | D | L | R deriving (Eq, Show)
data Guard = Guard {pos :: Coord, dir :: Dir, visited :: S.Set Coord} deriving (Eq, Show)

parse :: String -> (Map, Guard)
parse input =
    start $
        M.fromList
            [ (V2 j i, c)
            | (j, l) <- zip [0 ..] $ lines $ reverse input
            , (i, c) <- zip [0 ..] $ reverse l
            ]
  where
    start m = (m', Guard p U (S.singleton p))
      where
        (p, _) = M.findMin $ M.filter (== '^') m
        m' = M.update (const $ Just '.') p m

turnRight :: Dir -> Dir
turnRight = \case U -> R; R -> D; D -> L; L -> U

toStep :: Dir -> V2 Int
toStep = \case U -> V2 1 0; D -> V2 (-1) 0; R -> V2 0 1; L -> V2 0 (-1)

next :: Map -> Guard -> Guard
next m Guard{pos, dir, visited} = Guard pos' dir' visited'
  where
    is xy c = m M.!? xy == Just c
    forward = pos + toStep dir
    blocked = forward `is` '#'
    done = not $ M.member forward m
    dir' = if blocked then turnRight dir else dir
    pos' = if blocked || done then pos else forward
    visited' = if blocked || done then visited else S.insert pos' visited

goTilSame :: (Eq a) => (a -> a) -> a -> a
goTilSame f x = let y = f x in if y == x then x else goTilSame f y

part1 :: Map -> Guard -> Int
part1 m = S.size . visited . goTilSame (next m)

main :: IO ()
main = do
    (m, g) <- parse <$> readFile "input.txt"
    traverse_ (print . ($ (m, g)) . uncurry) [part1]
