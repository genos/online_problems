module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import           Control.Arrow ((&&&))
import           Data.Maybe    (fromMaybe)
import           Data.Vector   (Vector)
import qualified Data.Vector   as V

newtype Matrix a = M { _M :: Vector (Vector a) } deriving (Eq, Show)

cols :: Matrix a -> Int
cols = fromMaybe 0 . (V.!?0) . fmap length . _M

column :: Int -> Matrix a -> Vector a
column i = fmap (V.!i) . _M

flatten :: Matrix a -> Vector a
flatten = V.concat . V.toList . _M

fromList :: [[a]] -> Matrix a
fromList = M . V.fromList . fmap V.fromList

fromString :: Read a => String -> Matrix a
fromString = fromList . fmap (fmap read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (rs, cs) m =
  M . fmap (\r -> V.slice r cs $ flatten m) $ V.enumFromStepN 0 cs rs

row :: Int -> Matrix a -> Vector a
row i = (V.!i) . _M

rows :: Matrix a -> Int
rows = length . _M

shape :: Matrix a -> (Int, Int)
shape = rows &&& cols

transpose :: Matrix a -> Matrix a
transpose m = M . fmap (`column`m) . V.enumFromN 0 $ cols m
