{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.IO as T
import Linear.V2

type Coordinate = V2 Int
data Object = Sensor | Beacon deriving (Eq, Show)

readMap :: Text -> Map Coordinate Object
readMap = either (error "Bad parse") (M.fromList . concat) . parseOnly (row `sepBy1'` endOfLine)
  where
    row = (\x y -> [x, y]) <$> s <*> b
    s = (,Sensor) <$> ("Sensor at " *> c)
    b = (,Beacon) <$> (": closest beacon is at " *> c)
    c = V2 <$> ("x=" *> signed decimal) <*> (", y=" *> signed decimal)

main :: IO ()
main = do
    input <- readMap <$> T.readFile "test.txt"
    print input
