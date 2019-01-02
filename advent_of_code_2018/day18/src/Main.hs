{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Ix         (range)
import           Data.List       (iterate')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import           Data.Monoid     (Sum (..))
import           Linear.V2

data Acre = Open | Tree | Yard deriving (Eq, Ord)

type Forest = Map YX Acre

type YX = V2 Int

parseInput :: (Int, Int) -> (Int, Int) -> String -> Forest
parseInput ys xs input = M.fromList $ do
  (y, l) <- zip (range ys) $ lines input
  (x, c) <- zip (range xs) l
  let yx = V2 y x
  case c of
    '.' -> [(yx, Open)]
    '|' -> [(yx, Tree)]
    '#' -> [(yx, Yard)]
    _   -> []

counts :: Forest -> YX -> Map Acre Int
counts m yx =
  M.fromListWith (+) . fmap (, 1) . mapMaybe (m M.!?) . filter (/= yx) $ range
    (yx - pure 1, yx + pure 1)

step :: Forest -> Forest
step m = M.mapWithKey f m
 where
  f yx = \case
    Open | count Tree >= 3 -> Tree
         | otherwise       -> Open
    Tree | count Yard >= 3 -> Yard
         | otherwise       -> Tree
    Yard | count Yard >= 1 && count Tree >= 1 -> Yard
         | otherwise                          -> Open
    where count b = M.findWithDefault 0 b $ counts m yx

score :: Forest -> Int
score = g . foldMap f
 where
  g (Sum a, Sum b) = a * b
  f Open = mempty
  f Tree = (Sum 1, Sum 0)
  f Yard = (Sum 0, Sum 1)

part1 :: Forest -> Int
part1 = score . (!! 10) . iterate' step

data Loop = L { _timeToLoop :: {-# UNPACK #-}!Int, _size :: {-# UNPACK #-}!Int }

findLoop :: Ord a => (a -> a) -> a -> Loop
findLoop f x0 = go 1 (M.singleton x0 0) x0
 where
  go !i !seen !x =
    let x' = f x
    in  case seen M.!? x' of
          Nothing -> go (i + 1) (M.insert x' i seen) x'
          Just t  -> L t (i - t)

part2 :: Int -> Forest -> Int
part2 n m = score . (!! (extra + ttl)) $ iterate' step m
 where
  (L ttl size) = findLoop step m
  extra        = (n - ttl) `mod` size

main :: IO ()
main = do
  m <- parseInput (0, 49) (0, 49) <$> readFile "input"
  print $ part1 m
  print $ part2 1000000000 m
