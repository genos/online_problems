{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Attoparsec.Text hiding (D)
import Data.Foldable (foldl', traverse_)
import Data.List (scanl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Text (Text)
import qualified Data.Text.IO as T
import Linear.Metric (qd)
import Linear.V2
import Linear.Vector (unit)

data Direction = U | D | L | R deriving (Show)
data Move = Move {_dir :: !Direction, _steps :: {-# UNPACK #-} !Int} deriving (Show)

readMoves :: Text -> [Move]
readMoves = either (error "Bad parse") id . parseOnly (m `sepBy1'` endOfLine)
  where
    m = Move <$> d <*> (skipSpace *> decimal)
    d = choice [U <$ char 'U', D <$ char 'D', L <$ char 'L', R <$ char 'R']

type Coord = V2 Int
data Paths = Paths {_head :: NonEmpty Coord, _tail :: NonEmpty Coord}

d2c :: Direction -> Coord
d2c d = s d . unit $ xy d
  where
    s = \case U -> id; D -> negate; L -> negate; R -> id
    xy = \case U -> _y; D -> _y; L -> _x; R -> _x

m2c :: Move -> [Coord]
m2c (Move d s) = replicate s (d2c d)

start :: Paths
start = Paths (N.singleton 0) (N.singleton 0)

follow :: Coord -> Coord -> Coord
follow tv@(V2 tx ty) hv@(V2 hx hy) = if qd tv hv < 4 then tv else tv + V2 x y
  where
    x = signum $ hx - tx
    y = signum $ hy - ty

revPrep :: [a] -> NonEmpty a -> NonEmpty a
revPrep = N.prependList . reverse

move :: Paths -> Move -> Paths
move (Paths hs@(h :| _) ts@(t :| _)) m = Paths (revPrep hh hs) (revPrep tt ts)
  where
    hh = tail . scanl' (+) h $ m2c m
    tt = tail $ scanl' follow t hh

part1 :: [Move] -> Int
part1 = length . N.nub . _tail . foldl' move start

main :: IO ()
main = do
    moves <- readMoves <$> T.readFile "input.txt"
    traverse_ (print . ($ moves)) [part1]
