{-# LANGUAGE BangPatterns #-}
module Main
  ( main
  ) where

import           Data.IntMap.Strict (insert, (!?))
import qualified Relude.Unsafe      as U

input :: IO [Int]  -- copy-paste
input = pure [1, 0, 18, 10, 19, 6]

data LoopState = LoopState
  { _n          :: {-# UNPACK #-}!Int
  , _prev       :: {-# UNPACK #-}!Int
  , _collection :: !(IntMap Int)
  }

setup :: [Int] -> LoopState
setup !start =
  LoopState (length start) (U.last start) (fromList $ zip (U.init start) [1 ..])

go :: Int -> LoopState -> Int
go limit (LoopState !n !prev !collection)
  | n >= limit = prev
  | otherwise = go limit $ LoopState (n + 1)
                                     (maybe 0 (n -) (collection !? prev))
                                     (insert prev n collection)

part1 :: [Int] -> Int
part1 = go 2020 . setup

part2 :: [Int] -> Int
part2 = go 30000000 . setup

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
