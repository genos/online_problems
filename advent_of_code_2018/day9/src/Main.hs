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
import qualified Data.List.PointedList.Circular as PL
import           Data.Maybe                     (fromJust)
import qualified Data.Text.IO                   as T
import           Data.Vector.Unboxed            (Vector)
import qualified Data.Vector.Unboxed            as V

data Game = G { _scores :: !(Vector Int), _lastMarble :: {-# UNPACK #-}!Int }

numPlayers :: Game -> Int
numPlayers = V.length . _scores

make :: Int -> Int -> Game
make np = G (V.replicate np 0)

gameP :: P.Parser Game
gameP =
  make
    <$> (P.decimal <* P.string " players; last marble is worth ")
    <*> (P.decimal <* P.string " points")

move :: Int -> PointedList Int -> (Int, PointedList Int)
move p c | p `mod` 23 /= 0 = (0, PL.insertLeft p $ PL.moveN 2 c)
         | otherwise       = (p + PL._focus c', fromJust $ PL.deleteRight c')
  where c' = PL.moveN (-7) c

play :: Game -> Vector Int
play g =
  fst . foldl' go (_scores g, PL.singleton 0) $ zip moves pieces
 where
  go (!scores, !circle) (!turn, !piece) =
    first (\x -> scores & ix turn +~ x) $ move piece circle
  moves  = [i `mod` numPlayers g | i <- [0 ..]]
  pieces = [1 .. _lastMarble g]

part1 :: Game -> Int
part1 = V.maximum . play

part1Tests :: Vector Bool
part1Tests = V.zipWith3 (\np mp s -> s == part1 (make np mp))
                        players
                        lastMarble
                        scores
 where
  players    = V.fromList [10, 13, 17, 21, 30]
  lastMarble = V.fromList [1618, 7999, 1104, 6111, 5807]
  scores     = V.fromList [8317, 146373, 2764, 54718, 37305]

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
