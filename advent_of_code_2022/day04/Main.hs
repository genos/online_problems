module Main where

import Data.Attoparsec.Text
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Semigroup (Sum (..))
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
solve p = getSum . foldMap (\(x, y) -> bool 0 1 (p x y || p y x))

fullyContains :: Interval -> Interval -> Bool
fullyContains (Interval a b) (Interval c d) = a <= c && b >= d

overlaps :: Interval -> Interval -> Bool
overlaps (Interval a b) (Interval c d) = (a <= c && b >= c) || (a <= d && b >= d)

main :: IO ()
main = do
    pairs <- parseInput <$> T.readFile "input.txt"
    traverse_ (print . flip solve pairs) [fullyContains, overlaps]
