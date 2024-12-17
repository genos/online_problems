{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (find, traverse_)
import Data.List (elemIndex)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Alt (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Linear.V2

type Coord = V2 Int
data Spot = Box | Open | Closed deriving (Eq)
data Direction = U | D | L | R
data Warehouse = W {layout :: M.Map Coord Spot, robot :: Coord}

instance Show Spot where
    show Box = "O"; show Open = "."; show Closed = "#"

instance Show Warehouse where
    show (W m c) = unlines ls
      where
        V2 xMax yMax = fst $ M.findMax m
        f i j = if V2 i j == c then "@" else show $ m M.! V2 i j
        ls = [concatMap (`f` j) is | j <- js]
        is = [0 .. xMax]
        js = [0 .. yMax]

instance Show Direction where
    show U = "^"; show D = "v"; show L = "<"; show R = ">"

c2s :: Char -> Spot
c2s = ((M.fromList $ zip "O.@#" [Box, Open, Open, Closed]) M.!)

c2mi :: Char -> Maybe Direction
c2mi = ((M.fromList $ zip "^v<>" [U, D, L, R]) M.!?)

d2c :: Direction -> Coord
d2c = \case U -> V2 0 (-1); D -> V2 0 1; L -> V2 (-1) 0; R -> V2 1 0

parse :: Text -> (Warehouse, [Direction])
parse t = (w, ds)
  where
    w = W m (fromJust $ getAlt g)
    (m, g) = foldl' f (M.empty, mempty) . zip [0 ..] $ T.lines a
    ds = catMaybes $ T.foldr' ((:) . c2mi) [] b
    (a, b) = T.breakOn "\n\n" t
    f (m', g') (j, l) = (M.union m' $ M.fromList cs, g' <> g'')
      where
        l' = T.unpack l
        cs = [(V2 i j, c2s c) | (i, c) <- zip [0 ..] l']
        g'' = Alt ((`V2` j) <$> elemIndex '@' l')

step :: Warehouse -> Direction -> Warehouse
step (W m c) d = W m' c''
  where
    u = d2c d
    c' = c + u
    s = m M.! c'
    line = takeWhile (`M.member` m) $ iterate (+ u) c'
    push = find ((== Open) . (m M.!)) $ takeWhile ((/= Closed) . (m M.!)) line
    c'' = case (s, push) of (Box, Just _) -> c'; (Open, _) -> c'; _ -> c
    m' = case (s, push) of
        (Box, Just xy) -> M.adjust (const Open) c' $ M.adjust (const Box) xy m
        _ -> m

part1 :: (Warehouse, [Direction]) -> Int
part1 (w, ds) = sum . fmap gps . M.keys . M.filter (== Box) . layout $ foldl' step w ds
  where
    gps (V2 x y) = x + 100 * y

main :: IO ()
main = do
    input <- parse <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1]
