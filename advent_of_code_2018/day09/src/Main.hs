{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Control.Lens                   ((+~), ix)
import qualified Data.Attoparsec.Text           as P
import           Data.Bifunctor                 (first)
import           Data.Foldable                  (foldl')
import           Data.Function                  ((&))
import           Data.List.PointedList.Circular (PointedList (..))
import qualified Data.List.PointedList.Circular as C
import           Data.Maybe                     (fromJust)
import qualified Data.Text.IO                   as T
import           Data.Vector.Unboxed            (Vector)
import qualified Data.Vector.Unboxed            as V

data Game = G { _scores :: !(Vector Int), _lastMarble :: {-# UNPACK #-}!Int }

type Circle = PointedList Int

numPlayers :: Game -> Int
numPlayers = V.length . _scores

make :: Int -> Int -> Game
make np = G $ V.replicate np 0

gameP :: P.Parser Game
gameP =
  make
    <$> (P.decimal <* P.string " players; last marble is worth ")
    <*> (P.decimal <* P.string " points")

move :: Circle -> Int -> (Int, Circle)
move c p | p `mod` 23 /= 0 = (0, C.insertLeft p $ C.moveN 2 c)
         | otherwise       = (p + C._focus c', fromJust $ C.deleteRight c')
  where c' = C.moveN (-7) c

play :: Game -> Vector Int
play g =
  fst . foldl' go (_scores g, C.singleton 0) $ zip moves pieces
 where
  go (!ss, !circle) (!turn, !piece) =
    first (\points -> ss & ix turn +~ points) $ move circle piece
  moves  = [i `mod` numPlayers g | i <- [0 ..]]
  pieces = [1 .. _lastMarble g]

part1 :: Game -> Int
part1 = V.maximum . play

part1Tests :: Vector Bool
part1Tests = V.zipWith3 (\np mp s -> s == part1 (make np mp)) ps lm ss
 where
  ps = V.fromList [10, 13, 17, 21, 30]
  lm = V.fromList [1618, 7999, 1104, 6111, 5807]
  ss = V.fromList [8317, 146373, 2764, 54718, 37305]

part2 :: Game -> Int
part2 g = V.maximum . play $ g { _lastMarble = 100 * _lastMarble g }

main :: IO ()
main = do
  print $ V.and part1Tests
  input <- T.readFile "input"
  let game = case P.parseOnly gameP input of
        Left  l -> error l
        Right r -> r
  print $ part1 game
  print $ part2 game
