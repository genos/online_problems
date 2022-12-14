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
    expand a b = let d = signum (b - a) in a ^.. unfolded (\v -> bool (Just (v, v + d)) Nothing $  v - d == b)

top :: Coordinate
top = V2 500 0

candidates :: Coordinate -> [Coordinate]
candidates v = (v +) <$> [V2 0 1, V2 (-1) 1, V2 1 1]

solve :: (Cave -> Cave) -> Cave -> Int
solve sand cave = length $ sand cave `S.difference` cave

sand1 :: Cave -> Cave
sand1 cave = go cave top
  where
    offX = minimum1Of (folded . _x) cave
    offY = maximum1Of (folded . _y) cave
    go c v@(V2 x y)
        | x < offX || y > offY = c
        | otherwise =
            maybe (sand1 $ S.insert v c) (go c) $ findOf folded (`S.notMember` c) (candidates v)

sand2 :: Cave -> Cave
sand2 cave = go cave top
  where
    maxY = (2 +) $ maximum1Of (folded . _y) cave
    go c v@(V2 _ y)
        | S.member top c = c
        | y == maxY = sand2 (S.insert v c)
        | y > maxY = error "impossible"
        | otherwise =
            maybe (sand2 $ S.insert v c) (go c) $ findOf folded ((&&) <$> (`S.notMember` c) <*> ((<= maxY) . (^. _y))) (candidates v)

main :: IO ()
main = do
    input <- readCave <$> T.readFile "test.txt"
    traverse_ (print . (`solve` input)) [sand1, sand2]
