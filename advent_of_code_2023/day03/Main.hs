{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (foldMap')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Linear.V2

test :: Text
test =
    "467..114..\n\
    \...*......\n\
    \..35..633.\n\
    \......#...\n\
    \617*......\n\
    \.....+.58.\n\
    \..592.....\n\
    \......755.\n\
    \...$.*....\n\
    \.664.598.."

type Coord = V2 Int

data Schematic = S {numbers :: Map (Coord, Coord) Int, symbols :: Map Coord Char}

instance Semigroup Schematic where
    (S n0 p0) <> (S n1 p1) = S (M.union n0 n1) (M.union p0 p1)

instance Monoid Schematic where
    mempty = S mempty mempty

readSchematic :: Text -> Schematic
readSchematic = foldMap' f . zip [0 :: Int ..] . T.lines
  where
    f (i, t) = (\(_, _, ns, ss, _, _, _, _) -> S ns ss) $ T.foldl' g (i, 0, mempty, mempty, 0, 0, 0, False) t
    g (i, j, ns, ss, lo, hi, n, inNum) c
        | isDigit c && not inNum = (i, j + 1, ns, ss, j, j, digitToInt c, True)
        | isDigit c && inNum = (i, j + 1, ns, ss, lo, j, 10 * n + digitToInt c, True)
        | c /= '.' && inNum = (i, j + 1, M.insert (V2 i lo, V2 i hi) n ns, M.insert (V2 i j) c ss, j, j, 0, False)
        | c /= '.' && not inNum = (i, j + 1, ns, M.insert (V2 i j) c ss, j, j, 0, False)
        | c == '.' && inNum = (i, j + 1, M.insert (V2 i lo, V2 i hi) n ns, ss, j, j, 0, False)
        | otherwise = (i, j + 1, ns, ss, j, j, 0, False)

neighbors :: Coord -> [Coord]
neighbors (V2 x y) = [V2 (x + dx) (y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

adjToSymbol :: Schematic -> [Int]
adjToSymbol (S ns ss) = M.elems $ M.filterWithKey (\(lo, hi) _v -> any isAdj $ range lo hi) ns
  where
    range (V2 x y0) (V2 _x y1) = [V2 x y | y <- [y0 .. y1]]
    isAdj = any (`M.member` ss) . neighbors

part1 :: Schematic -> Int
part1 = sum . adjToSymbol

main :: IO ()
main = do
    input <- readSchematic <$> T.readFile "input.txt"
    print $ part1 $ readSchematic test
    print $ part1 input
