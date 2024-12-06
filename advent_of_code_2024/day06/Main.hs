module Main where

import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Linear.V2 (V2 (..))

type Coord = V2 Int
type Map = M.Map Coord Char
data Dir = U | D | L | R deriving (Eq, Ord, Show)
data Guard a = Guard {pos :: Coord, dir :: Dir, visited :: S.Set a} deriving (Eq, Show)

parse :: String -> Map
parse input =
    M.fromList
        [ (V2 j i, c)
        | (j, l) <- zip [0 ..] $ lines $ reverse input
        , (i, c) <- zip [0 ..] $ reverse l
        ]

start :: (Coord -> Dir -> a) -> Map -> (Map, Guard a)
start f m = (m', Guard p U (S.singleton $ f p U))
  where
    (p, _) = M.findMin $ M.filter (== '^') m
    m' = M.update (const $ Just '.') p m

turnRight :: Dir -> Dir
turnRight = \case U -> R; R -> D; D -> L; L -> U

toStep :: Dir -> V2 Int
toStep = \case U -> V2 1 0; D -> V2 (-1) 0; R -> V2 0 1; L -> V2 0 (-1)

next :: (Ord a) => (Coord -> Dir -> a) -> Map -> Guard a -> Guard a
next f m Guard{pos, dir, visited} = Guard pos' dir' visited'
  where
    is xy c = m M.!? xy == Just c
    ahead = pos + toStep dir
    blocked = ahead `is` '#'
    done = not $ M.member ahead m
    dir' = if blocked then turnRight dir else dir
    pos' = if blocked || done then pos else ahead
    visited' = if blocked || done then visited else S.insert (f pos' dir') visited

part1 :: Map -> Int
part1 m = length . visited $ go g
  where
    (m', g) = start const m
    go x = let y = next const m' x in if x == y then x else go y

part2 :: Map -> Int
part2 m = length . filter ((&&) <$> loops <*> (/= p0)) $ M.keys m'
  where
    f = (,)
    (m', g) = start f m
    p0 = pos g
    loops p = let m'' = M.update (const $ Just '#') p m'
               in undefined

main :: IO ()
main = do
    m <- parse <$> readFile "input.txt"
    traverse_ (print . ($ m)) [part1, part2]
