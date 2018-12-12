module Main where

import           Data.Foldable   (foldl', maximumBy)
import           Data.Ix         (range)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord        (comparing)
import           Linear.V2       (V2 (..))

type Cell  = V2 Int
type Table = Map Cell Int

main :: IO ()
main = do
  let g = makeTable 6878
  print $ part1 g
  print $ part2 g

part1 :: Table -> Cell
part1 table = fst . keyAndMaxValue $ allSubCellPower 3 table

part2 :: Table -> (Cell, Int)
part2 table =
  fst . maximumBy (comparing snd) . fmap (bestIn table) $ range (1, 300)

makeTable :: Int -> Table
makeTable serial = foldl'
  add
  M.empty
  [ (c, power serial c) | c <- range (pure 1, pure 300) ]

add :: Table -> (Cell, Int) -> Table
add table (c@(V2 x y), p) = M.insert c (p + upper + left - upperLeft) table
 where
  upper     = M.findWithDefault 0 (V2 (x - 1) y) table
  left      = M.findWithDefault 0 (V2 x (y - 1)) table
  upperLeft = M.findWithDefault 0 (V2 (x - 1) (y - 1)) table

power :: Int -> Cell -> Int
power _      (V2 0 _) = 0
power _      (V2 _ 0) = 0
power serial (V2 x y) = ((interim `div` 100) `mod` 10) - 5
 where
  rackID  = x + 10
  interim = (rackID * y + serial) * rackID

subCellPower :: Int -> Cell -> Table -> Int
subCellPower size (V2 x0 y0) t = f x y + f x' y' - f x y' - f x' y
 where
  f a b = M.findWithDefault 0 (V2 a b) t
  x  = x0 - 1
  x' = x + size
  y  = y0 - 1
  y' = y + size

allSubCellPower :: Int -> Table -> Table
allSubCellPower size table = M.fromList
  [ (c, subCellPower size c table) | c <- range (pure 1, pure (301 - size)) ]

keyAndMaxValue :: Ord b => M.Map a b -> (a, b)
keyAndMaxValue m = M.foldrWithKey mergeKV (M.findMin m) m
 where
  mergeKV k v (bestK, bestV) = if v > bestV then (k, v) else (bestK, bestV)

bestIn :: Table -> Int -> ((Cell, Int), Int)
bestIn table size = ((V2 x y, size), p)
  where (V2 x y, p) = keyAndMaxValue $ allSubCellPower size table
