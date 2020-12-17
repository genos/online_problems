module Main
  ( main
  ) where

import           Data.Foldable     (maximum)
import           Data.Ix           (inRange)
import qualified Data.Map.Strict   as M
import           Linear            (V2 (..), _x, _y)
import           Relude.Extra.Lens ((^.))
import qualified Relude.Unsafe     as U

data Seat = Empty | Occupied deriving stock Eq
type Point = V2 Int

input :: IO (Map Point Seat)
input = do
  raw <- fmap toString . lines <$> readFileText "input.txt"
  pure $ fromList
    [ (V2 x y, s)
    | (x, line) <- zip [0 ..] raw
    , (y, c   ) <- zip [0 ..] line
    , s         <- [ Empty | c == 'L' ]
    ]

fixed :: Eq a => (a -> a) -> a -> a
fixed f x = let y = f x in if x == y then x else fixed f y

count :: Map Point Seat -> Int
count = M.foldr' ((+) . bool 0 1 . (== Occupied)) 0

neighbors :: [Point]
neighbors = [ V2 x y | x <- [-1 .. 1], y <- bool [-1, 1] [-1 .. 1] (x /= 0) ]

rule :: Int -> (Point -> [Seat]) -> Point -> Seat -> Seat
rule n see xy p | p == Occupied && n <= ox xy             = Empty
                | p == Empty && notElem Occupied (see xy) = Occupied
                | otherwise                               = p
  where ox = length . filter (== Occupied) . see

step1 :: Map Point Seat -> Map Point Seat
step1 l = M.mapWithKey (rule 4 adj) l
  where adj xy = mapMaybe ((l M.!?) . (+ xy)) neighbors

part1 :: Map Point Seat -> Int
part1 = count . fixed step1

step2 :: Map Point Seat -> Map Point Seat
step2 l = M.mapWithKey (rule 5 see) l
 where
  see xy = mapMaybe ((l M.!?) <=< from xy) neighbors
  from xy n = find (`M.member` l) . takeWhile aSeat . U.tail $ iterate (+ n) xy
  aSeat = inRange (V2 0 0, V2 maxX maxY)
  maxX  = maximum . fmap (^. _x) $ M.keys l
  maxY  = maximum . fmap (^. _y) $ M.keys l

part2 :: Map Point Seat -> Int
part2 = count . fixed step2

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
