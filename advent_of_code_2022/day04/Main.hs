module Main where

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Word (Word64)

data Interval = Interval {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

parseInput :: Text -> [(Interval, Interval)]
parseInput = either (error "Bad parse") id . parseOnly (pair `sepBy1'` endOfLine)
  where
    pair = (,) <$> interval <*> (char ',' *> interval)
    interval = Interval <$> decimal <*> (char '-' *> decimal)

solve :: (Interval -> Interval -> Bool) -> [(Interval, Interval)] -> Int
solve p = length . filter (\(x, y) -> p x y || p y x)

contains :: Interval -> Interval -> Bool
contains (Interval a b) (Interval c d) = a <= c && b >= d

overlaps :: Interval -> Interval -> Bool
overlaps (Interval a b) (Interval c d) = (a <= c && b >= c) || (a <= d && b >= d)

main :: IO ()
main = do
    pairs <- parseInput <$> T.readFile "input.txt"
    traverse_ (print . ($ pairs) . solve) [contains, overlaps]
