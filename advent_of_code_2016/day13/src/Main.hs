{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import qualified Data.Set as S

type Key = Int
type Position = (Int, Int)

open :: Key -> Position -> Bool
open k = even . popCount . (+ k) . f
  where f (x, y) = x * x + 3 * x + 2 * x * y + y + y * y

neighbors :: Key -> Position -> [Position]
neighbors k (x, y) =
  filter ok [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] where
    ok (a, b) = (a >= 0) && (b >= 0) && open k (a, b)

bfs :: (Semigroup a, Monoid a, Ord b)
    => (b -> [b])     -- ^ calculates next positions to search
    -> (b -> Bool)    -- ^ are we done?
    -> (Set b -> a)   -- ^ score our current effort
    -> Int            -- ^ max depth of search allowed
    -> b              -- ^ initial position
    -> a
bfs candidates done score depth init = rec S.empty (S.singleton init) 0 where
  rec visited frontier steps
    | steps >= depth    = score frontier
    | any done frontier = mempty
    | otherwise         = score frontier <> rec v f s
      where
        s = steps + 1
        v = visited `S.union` frontier
        n = S.fromList $ concatMap candidates (S.toList frontier)
        f = n S.\\ v


part1 :: Key -> Position -> Position -> Sum Int
part1 key end = bfs (neighbors key) (== end) (const $ Sum 1) 100

part2 :: Key -> Position -> Sum Int
part2 key = bfs (neighbors key) (const False) (Sum . length) 50

main :: IO ()
main = do
  print . getSum $ part1 10 (7, 4) (1, 1)
  print . getSum $ part1 1350 (31, 39) (1, 1)
  print . getSum $ part2 1350 (1, 1)
