module Main where

import           Control.Arrow        ((&&&))
import           Control.Lens         ((^.))
import qualified Data.Attoparsec.Text as P
import           Data.Foldable        (traverse_)
import           Data.Ix              (range)
import           Data.List            (sortOn)
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

near :: [V] -> V -> [V]
near vs v = [ c | d0 /= d1 ]
 where
  [(d0, c), (d1, _c)] = take 2 . sortOn fst $ fmap (taxicab v &&& id) vs

part1 :: [V] -> Int
part1 vs = maximum . tally . concatMap (near vs) . filter finite $ range (l, u)
 where
  tally  = M.fromListWith (+) . fmap (id &&& const 1)
  (l, u) = bounds 0 vs
  finite (V2 x y) =
    and $ zipWith (/=) [x, y, x, y] [l ^. _x, l ^. _y, u ^. _x, u ^. _y]

part2 :: [V] -> Int
part2 vs = length . filter ((< n) . cabsTotal) . range $ bounds n' vs
 where
  n      = 10000
  (l, u) = bounds n vs
  n'     = max 0 (n - taxicab l u)
  cabsTotal v = sum $ fmap (taxicab v) vs

main :: IO ()
main = do
  input <- T.readFile "input"
  let vecs = case P.parseOnly (vec `P.sepBy` P.char '\n') input of
        Left  err -> error err
        Right vs  -> vs
  traverse_ (print . ($ vecs)) [part1, part2]
