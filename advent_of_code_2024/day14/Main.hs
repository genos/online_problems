{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (maximumBy, traverse_)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.IO qualified as T
import Linear.V2

data Robot = R {_p :: V2 Int, _v :: V2 Int} deriving (Eq, Show)
xMax, yMax, xMid, yMid :: Int
xMax = 101
yMax = 103
xMid = xMax `div` 2
yMid = yMax `div` 2
lim :: V2 Int
lim = V2 xMax yMax

parse_ :: Text -> [Robot]
parse_ = either (error "Bad parse") id . parseOnly (robot `sepBy1'` "\n")
  where
    robot = R <$> ("p=" *> v2) <*> (" v=" *> v2)
    v2 = V2 <$> signed decimal <*> ("," *> signed decimal)

step :: [Robot] -> [Robot]
step = fmap (\(R p v) -> R (liftA2 mod (p + v) lim) v)

part1 :: [Robot] -> Int
part1 rs = uL' * uR' * dR' * dL'
  where
    rs' = (!! 100) $ iterate step rs
    (uL', uR', dR', dL') = foldl' f (0, 0, 0, 0) rs'
    f (uL, uR, dR, dL) (R (V2 x y) _) =
        case (compare x xMid, compare y yMid) of
            (EQ, _) -> (uL, uR, dR, dL)
            (_, EQ) -> (uL, uR, dR, dL)
            (LT, LT) -> (uL, uR, dR, dL + 1)
            (GT, LT) -> (uL, uR, dR + 1, dL)
            (GT, GT) -> (uL, uR + 1, dR, dL)
            (LT, GT) -> (uL + 1, uR, dR, dL)

-- help from https://work.njae.me.uk/2024/12/14/advent-of-code-2024-day-14/
-- look for lots of diagonals
part2 :: [Robot] -> Int
part2 = fst . maximumBy (comparing snd) . zip [0 .. xMax * yMax] . fmap (numDiags . fmap _p) . iterate step
  where
    numDiags ps = length $ filter ok ps
      where
        ok (V2 x y) = V2 (x - 1) (y + 1) `S.member` set && V2 (x - 2) (y + 2) `S.member` set
        set = S.fromList ps

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
