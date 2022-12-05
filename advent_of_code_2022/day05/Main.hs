{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (foldl', traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V

type Stacks = Vector Text

{-
            [G] [W]         [Q]
[Z]         [Q] [M]     [J] [F]
[V]         [V] [S] [F] [N] [R]
[T]         [F] [C] [H] [F] [W] [P]
[B] [L]     [L] [J] [C] [V] [D] [V]
[J] [V] [F] [N] [T] [T] [C] [Z] [W]
[G] [R] [Q] [H] [Q] [W] [Z] [G] [B]
[R] [J] [S] [Z] [R] [S] [D] [L] [J]
 1   2   3   4   5   6   7   8   9
 -}
theStacks :: Stacks
theStacks = V.fromList ["ZVTBJGR", "LVRJ", "FQS", "CQVFLNHZ", "WMSCJTQR", "FHCTWS", "JNFVCZD", "QFRWDZGL", "PVWBJ"]

data Move = Move {_num :: {-# UNPACK #-} !Int, _from :: {-# UNPACK #-} !Int, _to :: {-# UNPACK #-} !Int}

parseMoves :: Text -> [Move]
parseMoves = either (error "Bad parse") id . parseOnly (move `sepBy1'` endOfLine)
  where
    move = Move <$> (string "move " *> decimal) <*> (string " from " *> decimal) <*> (string " to " *> decimal)

apply :: (Text -> Text) -> Stacks -> Move -> Stacks
apply f stacks (Move num from to) = stacks'
  where
    (from', to') = (pred from, pred to)
    (xs, ys) = T.splitAt num (stacks V.! from')
    zs = f xs <> (stacks V.! to')
    stacks' = stacks V.// [(to', zs), (from', ys)]

solve :: (Text -> Text) -> [Move] -> Text
solve f = V.foldl' (<>) T.empty . V.map (T.take 1) . foldl' (apply f) theStacks

main :: IO ()
main = do
    moves <- parseMoves <$> T.readFile "moves.txt"
    traverse_ (T.putStrLn . (`solve` moves)) [T.reverse, id]
