{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (traverse_)
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Data.Vector.Unboxed  (Vector)
import qualified Data.Vector.Unboxed  as V

type Probe = (Int, Int, Int, Int)
type Target = (Int, Int, Int, Int)
type Trajectory = Vector Probe

readTarget :: Text -> Target
readTarget = fromRight (error "Bad parse") . parseOnly p
 where
  p =
    (,,,)
      <$> ("target area: x=" *> decimal)
      <*> (".." *> decimal)
      <*> (", y=" *> signed decimal)
      <*> (".." *> signed decimal)

within :: Target -> Probe -> Bool
within (l, r, b, t) (x, y, _, _) = x >= l && x <= r && y >= b && y <= t

missed :: Target -> Probe -> Bool
missed (_, r, b, _) (x, y, _, _) = x > r || y < b

step :: Probe -> Probe
step (x, y, vx, vy) = (x + vx, y + vy, vx - 1 * signum vx, vy - 1)

trajectory :: Target -> Int -> Int -> Trajectory
trajectory target vx0 vy0 = V.snoc <*> (step . V.last) $ V.unfoldr go v0
 where
  v0 = (0, 0, vx0, vy0)
  go v = if within target v || missed target v then Nothing else Just (v, step v)

zenith :: Trajectory -> Int
zenith = V.maximum . V.map (\(_, y, _, _) -> y)

candidates :: Target -> [Trajectory]
candidates target@(l, r, b, _) = filter (within target . V.last)
  [ trajectory target vx vy | vx <- [(min 1 l) .. r], vy <- [b .. -b] ]

solve :: ([Vector Probe] -> Int) -> Target -> Int
solve finish = finish . candidates

part1 :: Target -> Int
part1 = solve (maximum . fmap zenith)

part2 :: Target -> Int
part2 = solve length

main :: IO ()
main = do
  target <- readTarget <$> T.readFile "input.txt"
  traverse_ (print . ($ target)) [part1, part2]
