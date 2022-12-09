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
data Paths = Paths { _head :: NonEmpty Coord, _tail :: NonEmpty Coord }

start :: Paths
start = Paths (N.singleton 0) (N.singleton 0)

move :: Paths -> Move -> Paths
move (Paths hs@(h :| _) ts@(t :| _)) (Move d s) = Paths (N.prependList (reverse hh) hs) (N.prependList (reverse tt) ts)
  where
    u = case d of
        U -> unit _y
        D -> negate $ unit _y
        L -> negate $ unit _x
        R -> unit _x
    hms = replicate s u
    hh = tail $ scanl' (+) h hms
    tt = tail $ scanl' f t hh
    f tv@(V2 tx ty) hv@(V2 hx hy) =
        let z = qd tv hv
            xx = signum $ hx - tx
            yy = signum $ hy - ty
         in if z < 4 then tv else tv + V2 xx yy

part1 :: [Move] -> Int
part1 = length . N.nub . _tail . foldl' move start

main :: IO ()
main = do
    moves <- readMoves <$> T.readFile "input.txt"
    traverse_ (print . ($ moves)) [part1]
