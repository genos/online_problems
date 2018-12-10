{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Circle                      (Circle)
import qualified Circle                      as C
import qualified Data.Attoparsec.Text        as P
import           Data.Foldable               (foldl')
import qualified Data.Text.IO                as T
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as V
import           Data.Vector.Unboxed.Mutable (write)

data Game = G { _numPlayers :: {-# UNPACK #-}!Int, _maxPoints :: {-# UNPACK #-}!Int }

gameP :: P.Parser Game
gameP =
  G
    <$> (P.decimal <* P.string " players; last marble is worth ")
    <*> (P.decimal <* P.string " points")

place :: Int -> Circle Int -> (Int, Circle Int)
place n c
  | n `mod` 23 == 0
  = let c' = C.moveLeft 7 c
        f  = C._focus c'
    in  (f, C.deleteRight c')
  | otherwise
  = (0, C.insertLeft n $ C.moveRight 2 c)

run :: Game -> Vector Int
run (G numPlayers maxPiece) = fst . foldl' go (V.replicate numPlayers 0, C.singleton 0)
                            $ zip players pieces
  where
    go (!scores, !circle) (!p, !i) =
      let (new, circle') = place i circle
          old            = V.unsafeIndex scores p
       in (V.modify (\s -> write s p (old + new)) scores, circle')
    players  = (`mod` numPlayers) <$> [0 ..]
    pieces = [1..maxPiece]

main :: IO ()
main = do
  input <- T.readFile "input"
  let game = case P.parseOnly gameP input of
        Left  l -> error l
        Right r -> r
  print . V.maximum $ run game
