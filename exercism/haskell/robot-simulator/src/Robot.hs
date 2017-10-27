module Robot
    ( Bearing(..)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

import Control.Arrow (first, second)
import Data.Bool     (bool)
import Data.Foldable (foldl')

data Bearing = North | East | South | West deriving (Bounded, Eq, Enum, Show)

data Robot = Robot { bearing :: Bearing, coordinates :: (Integer, Integer) }

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate = foldl' execute
 where
  execute :: Robot -> Char -> Robot
  execute (Robot b c) 'L' = Robot (turnLeft b) c
  execute (Robot b c) 'R' = Robot (turnRight b) c
  execute (Robot b c) 'A' = Robot b $ advance b c
  execute _           c   = error $ "Bad instruction: " ++ [c]
  advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
  advance North = second succ
  advance South = second pred
  advance East  = first succ
  advance West  = first pred

turnLeft :: Bearing -> Bearing
turnLeft b = bool (pred b) maxBound (b == minBound)

turnRight :: Bearing -> Bearing
turnRight b = bool (succ b) minBound (b == maxBound)
