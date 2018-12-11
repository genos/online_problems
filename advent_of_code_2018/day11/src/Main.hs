module Main where

import           Control.Arrow   ((&&&))
import           Data.Foldable   (maximumBy)
import           Data.Ix         (range)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord        (comparing)
import           Linear.V2       (V2 (..))
import           Data.Vector     (Vector)
import qualified Data.Vector     as V

type Cell  = V2 Int
type Table = Map Cell Int

minCell :: Cell
minCell = V2 1 1

maxSize :: Int
maxSize = 300

maxCell :: Cell
maxCell = V2 maxSize maxSize

grid :: [Cell]
grid = range (minCell, maxCell)

power :: Int -> Cell -> Int
power serial (V2 x y) = hundreds - 5
 where
  rackID   = x + 10
  level    = (rackID * y + serial) * rackID
  hundreds = (level `div` 100) `mod` 10
{-# INLINE power #-}

testPower :: Bool
testPower = and $ zipWith3 (\c s p -> power s c == p)
                           [V2 122 79, V2 217 196, V2 101 153]
                           [57, 39, 71]
                           [-5, 0, 4]

powerTable :: Int -> Table
powerTable serial = M.fromList $ fmap (id &&& power serial) grid

neighbors :: Int -> Cell -> Vector Cell
neighbors size c@(V2 x y) = V.fromList $ range (c, V2 x' y')
  where
    x' = min maxSize (x + size)
    y' = min maxSize (y + size)

largest :: Int -> Int -> Cell
largest serial size = maximumBy (comparing totalPower) grid
 where
  table        = powerTable serial
  totalPower c = sum $ fmap (table M.!) (neighbors size c)

part1 :: String
part1 = show x <> "," <> show y
  where (V2 x y) = largest 6878 2

bestInTable :: Table -> Cell
bestInTable table = maximumBy (comparing (table M.!)) grid

powerTables :: Int -> Vector Table
powerTables serial = V.unfoldrN maxSize f (1, p)
  where
    p = powerTable serial
    f :: (Int, Table) -> Maybe (Table, (Int, Table))
    f (size, table) = Just (table, (size + 1, M.fromList $ fmap g grid))
      where
        g :: Cell -> (Cell, Int)
        g cell = (cell, sum $ fmap (p M.!) (neighbors (size - 1) cell))

largest' :: Int -> (Cell, Int)
largest' serial = (cell, size)
  where
    cell   = fst $ pairs V.! size
    size   = V.maxIndex pairs
    pairs  = V.zipWith (\t b -> (b, t M.! b)) tables bests
    bests  = fmap bestInTable tables
    tables = powerTables serial

testLargest' :: Bool
testLargest' = and $ zipWith3
  (\serial cell size -> largest' serial == (cell, size))
  [18, 42]
  [V2 90 269, V2 232 251]
  [16, 12]

part2 :: String
part2 = show x <> "," <> show y <> "," <> show size
  where (V2 x y, size) = largest' 6878

main :: IO ()
main = do
  print testPower
  putStrLn part1
  print testLargest'
  putStrLn part2
