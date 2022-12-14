{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Data.Attoparsec.Text hiding (take)
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Linear.V2

data Space = Rock | Sand deriving (Eq)
type Coordinate = V2 Int
type Cave = Map Coordinate Space

readCave :: Text -> Cave
readCave = either (error "Bad parse") (M.fromList . concat) . parseOnly (path `sepBy1'` endOfLine)
  where
    path = fmap (,Rock) . concat . (zipWith expand <*> tail) <$> stops
    coord = V2 <$> decimal <*> (char ',' *> decimal)
    stops = coord `sepBy1'` " -> "
    expand a b = let d = b - a in take (succ . maximum $ abs d) $ iterate (+ signum d) a

top :: Coordinate
top = V2 500 0

solve :: (Cave -> Cave) -> Cave -> Int
solve sand = length . M.filter (== Sand) . sand

sand1 :: Cave -> Cave
sand1 cave = either sand1 id (go top cave)
  where
    keys = M.keys cave
    off_x = minimum1Of (folded . _x) keys
    off_y = maximum1Of (folded . _y) keys
    go v@(V2 x y) c
        | x < off_x || y > off_y = Right c -- fell off, all done
        | otherwise =
            let next = findOf folded (`M.notMember` c) [v + V2 0 1, v + V2 (-1) 1, v + V2 1 1]
             in if isNothing next
                    then Left (M.insert v Sand c) -- at rest, next sand
                    else go (fromJust next) c -- keep going with this one

main :: IO ()
main = do
    input <- readCave <$> T.readFile "input.txt"
    print $ solve sand1 input
