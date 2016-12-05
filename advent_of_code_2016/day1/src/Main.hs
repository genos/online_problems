module Main where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Safe (tailSafe)

data Direction = N | S | E | W
data Position = P { _x :: Int , _y :: Int , _d :: Direction }
data Turn = L | R deriving Read
type Blocks = Int
data Move = M { _t :: Turn , _b :: Blocks }

instance Read Move where
    readsPrec _ (t:b) = [ (M t' b', "") | (t `elem` "LR") && all isDigit b ]
      where
        t' = read [t] :: Turn
        b' = read b :: Blocks
    readsPrec _ _ = []

turn :: Position -> Turn -> Position
turn p t = p {_d = turn' (_d p) t} where
  turn' N L = W
  turn' N R = E
  turn' S L = E
  turn' S R = W
  turn' E L = N
  turn' E R = S
  turn' W L = S
  turn' W R = N

step :: Position -> Blocks -> Position
step (P x y N) b = P (x + b) y N
step (P x y S) b = P (x - b) y S
step (P x y E) b = P x (y + b) E
step (P x y W) b = P x (y - b) W

turnAndGo :: Position -> Move -> Position
turnAndGo p (M t b) = step (turn p t) b

distance :: Position -> Position -> Int
distance p1 p2 = abs (_x p2 - _x p1) + abs (_y p2 - _y p1)

steps :: Position -> Move -> [Position]
steps p (M t b) = tailSafe . scanl step (turn p t) $ replicate b 1

allSteps :: Position -> [Move] -> [Position]
allSteps p [] = [p]
allSteps p (m : ms) = ps ++ allSteps (last ps) ms where ps = steps p m

firstRepeat :: [Position] -> Maybe Position
firstRepeat = snd . foldl go (Set.empty, Nothing) where
  go (s, m) p | isJust m    = (s, m)
              | xy `elem` s = (s, Just p)
              | otherwise   = (xy `Set.insert` s, Nothing)
              where xy = (_x &&& _y) p

start :: Position
start = P 0 0 N

input :: [Move]
input = fmap read . words . filter (/= ',') $
    "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3"

part1 :: Int
part1 = distance start $ foldl turnAndGo start input

part2 :: Maybe Int
part2 = fmap (distance start) . firstRepeat $ allSteps start input

main :: IO ()
main = do
  print part1
  print part2
