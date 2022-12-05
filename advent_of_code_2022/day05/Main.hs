{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (foldl', traverse_)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V

type Stacks = Vector Text

data Move = Move {_num :: {-# UNPACK #-} !Int, _from :: {-# UNPACK #-} !Int, _to :: {-# UNPACK #-} !Int}

parseSetup :: Text -> (Stacks, [Move])
parseSetup = either (error "Bad parse") id . parseOnly ((,) <$> stacks <*> (skipStuff *> moves))
  where
    stacks = V.map T.stripStart . V.fromList <$> (T.transpose <$> (row `sepBy1'` endOfLine))
    row = T.concat <$> entry `sepBy1'` char ' '
    entry = choice [count 3 (char ' ') $> " ", T.singleton <$> (char '[' *> letter <* char ']')]
    skipStuff = endOfLine *> skipWhile (/= '\n') *> endOfLine *> endOfLine
    moves = move `sepBy1'` endOfLine
    move = Move <$> (string "move " *> decimal) <*> (string " from " *> decimal) <*> (string " to " *> decimal)

apply :: (Text -> Text) -> Stacks -> Move -> Stacks
apply f stacks (Move num from to) = stacks'
  where
    (from', to') = (pred from, pred to)
    (xs, ys) = T.splitAt num (stacks V.! from')
    zs = f xs <> (stacks V.! to')
    stacks' = stacks V.// [(to', zs), (from', ys)]

solve :: (Text -> Text) -> (Stacks, [Move]) -> Text
solve f (stacks, moves) = V.foldMap (T.take 1) $ foldl' (apply f) stacks moves

main :: IO ()
main = do
    (stacks, moves) <- parseSetup <$> T.readFile "input.txt"
    traverse_ (T.putStrLn . (`solve` (stacks, moves))) [T.reverse, id]
