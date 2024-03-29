{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Attoparsec.Text
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Linear.V2

type Coordinate = V2 Int
type Cave = Set Coordinate

readCave :: Text -> Cave
readCave = either (error "Bad parse") (S.fromList . concat) . parseOnly (path `sepBy1'` endOfLine)
  where
    path = concat . (zipWith expand <*> tail) <$> stops
    coord = V2 <$> decimal <*> (char ',' *> decimal)
    stops = coord `sepBy1'` " -> "
    expand a b = let d = signum (b - a) in a ^.. unfolded (\v -> bool (Just (v, v + d)) Nothing $ v - d == b)

top :: Coordinate
top = V2 500 0

sand :: Cave -> Cave
sand cave = go cave top
  where
    minX = minimum1Of (folded . _x) cave
    maxY = maximum1Of (folded . _y) cave
    go c v@(V2 x y)
        | top `S.member` c || x < minX || y > maxY = c
        | otherwise = maybe (sand $ S.insert v c) (go c) $ findOf folded (`S.notMember` c) ((v +) <$> [V2 0 1, V2 (-1) 1, V2 1 1])

solve :: Cave -> Int
solve cave = length $ sand cave `S.difference` cave

fillFloor :: Cave -> Cave
fillFloor cave = cave `S.union` S.fromList [V2 x y | x <- [tx - y .. tx + y]]
  where
    y = (2 +) $ maximum1Of (folded . _y) cave
    tx = top ^. _x

main :: IO ()
main = do
    input <- readCave <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [solve, solve . fillFloor]
