module Main where

import           Control.Arrow   ((&&&))
import           Data.Foldable   (foldl', maximumBy)
import           Data.Ix         (range)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord        (comparing)
import           Linear.V2       (V2 (..))

type Cell = V2 Int

minCell :: Cell
minCell = V2 1 1

maxCoord :: Int
maxCoord = 300

maxCell :: Cell
maxCell = V2 maxCoord maxCoord

grid :: [Cell]
grid = range (minCell, maxCell)

power :: Int -> Cell -> Int
power serial (V2 x y) = hundreds - 5
 where
  rackID   = x + 10
  level    = (rackID * y + serial) * rackID
  hundreds = (level `div` 100) `mod` 10

testPower :: Bool
testPower = and $ zipWith3 (\c s p -> power s c == p)
                           [V2 122 79, V2 217 196, V2 101 153]
                           [57, 39, 71]
                           [-5, 0, 4]

powerTable :: Int -> Map Cell Int
powerTable serial = M.fromList $ fmap (id &&& power serial) grid

sum' :: Foldable f => f Int -> Int
sum' = foldl' (+) 0

neighbors :: Int -> Cell -> [Cell]
neighbors size c@(V2 x y) = range (c, V2 x' y')
  where
    x' = min maxCoord (x + size)
    y' = min maxCoord (y + size)

largest :: Int -> Int -> Cell
largest serial size = maximumBy (comparing totalPower) grid
 where
  table        = powerTable serial
  totalPower c = sum' [ table M.! n | n <- neighbors size c ]

part1 :: String
part1 = show x <> "," <> show y
  where (V2 x y) = largest 6878 2

-- largest' :: Int -> (Cell, Int)
-- largest' serial = maximumBy (comparing $ tp)
--   [ (cell, size) | cell <- grid, size <- range (0, maxCoord) ]
--     where
--       pm = powerTable serial
--       totalPower c

-- part2 :: (Cell, Int)
-- part2 = largest' 6878

-- testLargest' :: Bool
-- testLargest' = and $ zipWith3
--   (\serial cell size -> largest' serial == (cell, size))
--   [18, 42]
--   [V2 90 269, V2 232 251]
--   [16, 12]

main :: IO ()
main = do
  print testPower
  putStrLn part1
  --print testLargest'
