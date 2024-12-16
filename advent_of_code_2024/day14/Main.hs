{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Linear.V2

data Robot = R {_p :: V2 Int, _v :: V2 Int} deriving (Eq, Show)

parse_ :: Text -> [Robot]
parse_ = either (error "Bad parse") id . parseOnly (robot `sepBy1'` "\n")
  where
    robot = R <$> ("p=" *> v2) <*> (" v=" *> v2)
    v2 = V2 <$> signed decimal <*> ("," *> signed decimal)

move :: V2 Int -> Robot -> Robot
move lim (R p v) = R (liftA2 mod (p + v) lim) v

step :: V2 Int -> [Robot] -> [Robot]
step lim = fmap (move lim)

part1 :: V2 Int -> [Robot] -> Int
part1 lim@(V2 xMax yMax) rs = uL' * uR' * dR' * dL'
  where
    rs' = (!! 100) $ iterate (step lim) rs
    xMid = xMax `div` 2
    yMid = yMax `div` 2
    (uL', uR', dR', dL') = foldl' f (0, 0, 0, 0) rs'
    f (uL, uR, dR, dL) (R (V2 x y) _) =
        case (compare x xMid, compare y yMid) of
            (EQ, _) -> (uL, uR, dR, dL)
            (_, EQ) -> (uL, uR, dR, dL)
            (LT, LT) -> (uL, uR, dR, dL + 1)
            (GT, LT) -> (uL, uR, dR + 1, dL)
            (GT, GT) -> (uL, uR + 1, dR, dL)
            (LT, GT) -> (uL + 1, uR, dR, dL)

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1 $ V2 101 103]
