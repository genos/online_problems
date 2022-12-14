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

data Space = Rock | Sand deriving Eq

instance Show Space where
    show Rock = "#"
    show Sand = "o"

type Coordinate = V2 Int
type Cave = Map Coordinate Space

readCave :: Text -> Cave
readCave = either (error "Bad parse") (M.fromList . concat) . parseOnly (path `sepBy1'` endOfLine)
  where
    coord = V2 <$> decimal <*> (char ',' *> decimal)
    stops = coord `sepBy1'` " -> "
    path = fmap (,Rock) . concat . (zipWith expand <*> tail) <$> stops
    expand a b =
        let d = b - a
            s = signum d
            n = maximum $ abs d
         in take (succ n) $ iterate (+ s) a

sand :: Coordinate -> Cave -> Either Cave Cave
sand xy cave = go xy cave
  where
    keys = M.keys cave
    off_x = minimum1Of (folded . _x) keys
    off_y = maximum1Of (folded . _y) keys
    go v@(V2 x y) c
        | x < off_x || y > off_y = Left c  -- fell off
        | otherwise =
            let next = findOf folded (`M.notMember` c) [v + V2 0 1, v + V2 (-1) 1, v + V2 1 1]
             in if isNothing next
                    then Right (M.insert v Sand c) -- at rest
                    else go (fromJust next) c  -- keep going

allSand :: Coordinate -> Cave -> Cave
allSand xy cave = case sand xy cave of
                    Left c' -> c'
                    Right c' -> allSand xy c'

part1 :: Cave -> Int
part1 = length . M.filter (== Sand) . allSand (V2 500 0)

main :: IO ()
main = do
    input <- readCave <$> T.readFile "input.txt"
    print $ part1 input
