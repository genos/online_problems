{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Linear.V2 (V2 (..))

data Square = Empty | Bug deriving (Eq, Ord, Show)

type Grid = Map (V2 Int) Square

fromC :: Char -> Maybe Square
fromC = \case '.' -> Just Empty; '#' -> Just Bug; _ -> Nothing

toC :: Square -> Char
toC = \case Empty -> '.'; Bug -> '#'

parse :: String -> Grid
parse s = M.fromList $ zip coords squares
  where
    ls = lines s
    n = fromIntegral $ length ls - 1 -- assumes square grid…
    coords = [V2 x y | x <- [0 .. n], y <- [0 .. n]]
    squares = concatMap (mapMaybe fromC) ls

readInput :: FilePath -> IO Grid
readInput = (parse <$>) . readFile

pretty :: Grid -> String
pretty g = unlines ss
  where
    V2 x y = fst $ M.findMax g
    coords = [[V2 x' y' | y' <- [0 .. y]] | x' <- [0 .. x]]
    ss = fmap (fmap (toC . (g M.!))) coords

next :: Grid -> Grid
next g = M.mapWithKey update g
  where
    update v s
      | s == Empty && (n == 1 || n == 2) = Bug
      | s == Bug && n == 1 = Bug
      | otherwise = Empty
      where
        adj = [v + w | w <- [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]]
        n = length . filter (== Bug) $ mapMaybe (g M.!?) adj

testNext :: Bool
testNext = and $ zipWith (==) (tail grids) (fmap next grids)
  where
    grids =
      fmap
        parse
        [ "....#\n#..#.\n#..##\n..#..\n#....",
          "#..#.\n####.\n###.#\n##.##\n.##..",
          "#####\n....#\n....#\n...#.\n#.###",
          "#....\n####.\n...##\n#.##.\n.##.#",
          "####.\n....#\n##..#\n.....\n##..."
        ]

firstRepeat :: (Eq a, Ord a) => (a -> a) -> a -> a
firstRepeat f = go S.empty
  where
    go !xs !x
      | S.member x xs = x
      | otherwise = go (S.insert x xs) (f x)

testFirstRepeat :: Bool
testFirstRepeat = h == firstRepeat next g
  where
    g = parse "....#\n#..#.\n#..##\n..#..\n#...."
    h = parse ".....\n.....\n.....\n#....\n.#..."

biodiversity :: Grid -> Int
biodiversity = snd . M.foldl' f (1, 0)
  where
    f (!ctr, !acc) !s = let x = if s == Bug then ctr else 0 in (2 * ctr, acc + x)

testBiodiversity :: Bool
testBiodiversity = 2129920 == biodiversity (parse ".....\n.....\n.....\n#....\n.#...")

part1 :: IO Int
part1 = biodiversity . firstRepeat next <$> readInput "input_1"

main :: IO ()
main = do
  putStrLn . pretty =<< readInput "input_1"
  print $ testNext && testFirstRepeat && testBiodiversity
  print =<< part1
