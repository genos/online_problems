{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (elemIndex)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Linear.V2

type Coord = V2 Int
data Spot = Box | Open | Closed deriving (Show)
type Map = M.Map Coord Spot
data Instruction = U | D | L | R deriving (Show)

makeC2X :: String -> [a] -> (Char -> a)
makeC2X s as = (m M.!) where m = M.fromList $ zip s as

c2s :: Char -> Spot
c2s = makeC2X "O.@#" [Box, Open, Open, Closed]

c2i :: Char -> Instruction
c2i = makeC2X "^v<>" [U, D, L, R]

parse :: Text -> (Map, Coord, [Instruction])
parse t = (m, fromJust $ getFirst g, is)
  where
    (m, g) = foldl' f (M.empty, mempty) . zip [0 ..] $ T.lines left
    is = concatMap (T.foldr' ((:) . c2i) []) $ T.lines right
    (left, right) = T.breakOn "\n\n" t
    f (m', g') (j, l) = (M.union m' $ M.fromList cs, g' <> g'')
      where
        l' = T.unpack l
        cs = [(V2 i j, c2s c) | (i, c) <- zip [0 ..] l']
        g'' = First ((`V2` j) <$> elemIndex '@' l')

main :: IO ()
main = T.putStrLn "TODO"
