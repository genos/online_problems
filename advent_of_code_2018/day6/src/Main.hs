module Main where

import           Control.Arrow        ((&&&))
import           Control.Lens         ((^.))
import qualified Data.Attoparsec.Text as P
import           Data.Ix              (range)
import           Data.List            (sortOn)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import qualified Data.Text.IO         as T
import           Linear.V2

type V = V2 Int

vec :: P.Parser V
vec = V2 <$> P.decimal <*> (P.char ',' *> P.space *> P.decimal)

taxicab :: V -> V -> Int
taxicab v w = sum . abs $ v - w

bounds :: Int -> [V] -> (V, V)
bounds n vs = (a - nv, b + nv)
  where
    xs = fmap (^. _x) vs
    ys = fmap (^. _y) vs
    a  = V2 (minimum xs) (minimum ys)
    b  = V2 (maximum xs) (maximum ys)
    nv = V2 n n

grid :: Int -> [V] -> [V]
grid n = range . bounds n

closest :: [V] -> V -> [V]
closest vs v = [ c0 | d0 /= d1 ]
 where
  [(d0, c0), (d1, _c1)] = take 2 . sortOn fst $ fmap (taxicab v &&& id) vs

tally :: (Ord a) => [a] -> Map a Word
tally = M.fromListWith (+) . fmap (id &&& const 1)

part1 :: [V] -> Word
part1 vs = maximum . tally . concatMap (closest vs) $ grid 0 vs

totalCabs :: [V] -> V -> Int
totalCabs vs v = sum $ fmap (taxicab v) vs

part2 :: [V] -> Int
part2 vs = length inRange
  where
    n = 100000
    (l, u) = bounds n vs
    inRange = filter ((< 10000) . totalCabs vs) $ grid (max 0 (n - taxicab l u)) vs

main :: IO ()
main = do
  input <- T.readFile "input"
  let vecs = case P.parseOnly (vec `P.sepBy` P.char '\n') input of
        Left  err -> error err
        Right vs  -> vs
  print $ part1 vecs
  print $ part2 vecs
