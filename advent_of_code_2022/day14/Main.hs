{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Data.Attoparsec.Text hiding (take)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
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

candidates :: Coordinate -> [Coordinate]
candidates v = (v +) <$> [V2 0 1, V2 (-1) 1, V2 1 1]

solve :: (Cave -> Cave) -> Cave -> Int
solve sand = length . M.filter (== Sand) . sand

sand1 :: Cave -> Cave
sand1 cave = go cave top
  where
    keys = M.keys cave
    offX = minimum1Of (folded . _x) keys
    offY = maximum1Of (folded . _y) keys
    go c v@(V2 x y)
        | x < offX || y > offY = c
        | otherwise =
            let next = findOf folded (`M.notMember` c) (candidates v)
             in maybe (sand1 $ M.insert v Sand c) (go c) next

sand2 :: Cave -> Cave
sand2 cave = go cave top
  where
    maxY = (2 +) $ maximum1Of (folded . _y) (M.keys cave)
    go c v
        | M.member top c = c
        | otherwise =
            let next = findOf folded (`M.notMember` c) . filter ((<= maxY) . (^. _y)) $ candidates v
             in maybe (sand2 $ M.insert v Sand c) (go c) next

main :: IO ()
main = do
    input <- readCave <$> T.readFile "test.txt"
    traverse_ (print . (`solve` input)) [sand1, sand2]
