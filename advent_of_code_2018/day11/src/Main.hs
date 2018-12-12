{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Arrow   ((&&&))
import           Data.Bifunctor  (bimap)
import           Data.Bool       (bool)
import           Data.Foldable   (foldl', maximumBy, traverse_)
import           Data.Ix         (range)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord        (comparing)
import           Linear.V2       (V2 (..))

type Cell = V2 Int

type Table = Map Cell Int

rangeC :: (Int, Int) -> [Cell]
rangeC = range . bimap pure pure
{-# INLINE rangeC #-}

main :: IO ()
main = do
  let t = makeTable 6878
  traverse_ (putStrLn . ($ t)) [part1, part2]

part1 :: Table -> String
part1 table = show x <> "," <> show y
  where (V2 x y) = fst . argMax $ summedAreaTable 3 table

part2 :: Table -> String
part2 table = show x <> "," <> show y <> "," <> show size
 where
  (V2 x y, size) =
    fst . maximumBy (comparing snd) . fmap best $ range (1, 300)
  best s =
    let (c, p) = argMax $ summedAreaTable s table in ((c, s), p)

makeTable :: Int -> Table
makeTable serial =
  foldl' add M.empty . fmap (id &&& power serial) $ rangeC (1, 300)
 where
  add table (c, p) = M.insert c (p + q) table
   where
    !q = sum [f a, f b, -f d]
    !a = c - V2 1 0
    !b = c - V2 0 1
    !d = c - V2 1 1
    f  = flip (M.findWithDefault 0) table
    {-# INLINE f #-}

power :: Int -> Cell -> Int
power serial (V2 x y) = h - 5
 where
  !rackID = x + 10
  !h      = (((rackID * y + serial) * rackID) `div` 100) `mod` 10
{-# INLINE power #-}

summedAreaTable :: Int -> Table -> Table
summedAreaTable size table =
  M.fromList . fmap (id &&& subCellPower) $ rangeC (1, 301 - size)
    where
      subCellPower :: Cell -> Int
      subCellPower cell = sum [f c, f a, -f b, -f d]
        where
          !c = cell - pure 1
          !a = c + pure size
          !b = c + V2 0 size
          !d = c + V2 size 0
          f  = flip (M.findWithDefault 0) table
          {-# INLINE f #-}

argMax :: Ord b => M.Map a b -> (a, b)
argMax m = M.foldrWithKey' f (M.findMin m) m
 where
  f k v (k', v') = bool (k', v') (k, v) (v > v')
  {-# INLINE f #-}
