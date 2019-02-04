{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Int (Int64)
import Data.Ord (comparing)
import qualified Data.SBV as S
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V

-- set up
data Nanobot = Nanobot
  { _x, _y, _z, _r :: {-# UNPACK #-}!Int64
  } deriving (Show)

nanobot :: Parser Nanobot
nanobot = do
  _x <- string "pos=<" *> signed decimal
  _y <- char ',' *> signed decimal
  _z <- char ',' *> signed decimal
  _r <- string ">, r=" *> signed decimal
  pure Nanobot {_x, _y, _z, _r}

-- part 1
dist :: Nanobot -> Nanobot -> Int64
dist (Nanobot !x0 !y0 !z0 _) (Nanobot !x1 !y1 !z1 _) =
  abs (x0 - x1) + abs (y0 - y1) + abs (z0 - z1)

{-# INLINE dist #-}
inRange :: Nanobot -> Nanobot -> Bool
inRange a b = dist a b <= _r a

{-# INLINE inRange #-}
part1 :: Vector Nanobot -> Int
part1 bots = V.length $! V.filter (inRange strongest) bots
  where
    strongest = V.maximumBy (comparing _r) bots

-- part 2
type S64 = S.SBV Int64

absS :: S64 -> S64
absS n = S.ite (n S..< 0) (negate n) n

distS :: S64 -> S64 -> S64 -> S64 -> S64 -> S64 -> S64
distS x0 y0 z0 x1 y1 z1 = absS (x0 - x1) + absS (y0 - y1) + absS (z0 - z1)

inRangeS :: S64 -> S64 -> S64 -> Nanobot -> S64
inRangeS x y z (Nanobot nx ny nz nr) =
  S.oneIf . (S..<= S.literal nr) $!
  distS (S.literal nx) (S.literal ny) (S.literal nz) x y z

part2 :: Vector Nanobot -> IO S.OptimizeResult
part2 bots =
  S.optimize S.Lexicographic $ do
    [x, y, z] <- S.sInt64s ["x", "y", "z"]
    S.maximize "nanobots-in-range" . sum $ fmap (inRangeS x y z) bots
    S.minimize "distance-to-origin" $ distS 0 0 0 x y z

-- main
main :: IO ()
main = do
  input <- T.readFile "input"
  let bots =
        V.fromList $!
        either error id $! parseOnly (nanobot `sepBy` char '\n') input
  print $! part1 bots
  -- print =<< part2 bots  -- SLOW
