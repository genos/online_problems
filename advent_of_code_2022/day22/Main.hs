{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding (readFile)

import Data.Attoparsec.Text hiding (takeWhile)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Linear.V2

type Direction = V2 Int -- assumed to be ± unit vector
type Coord = V2 Int
data RL = L | R
data Move = Forward Int | Turn RL
data Square = Tile | Wall deriving (Eq)
type Board = Map Coord Square

readBoardAndMoves :: Text -> (Board, [Move])
readBoardAndMoves =
    either (error "Bad parse") (first M.fromList)
        . parseOnly ((,) <$> pBoard <*> (endOfLine *> endOfLine *> pMoves))
  where
    pBoard = entries <$> (pLine `sepBy1'` endOfLine)
    pLine = many1' pSquare
    pSquare = choice [Nothing <$ char ' ', Just Tile <$ char '.', Just Wall <$ char '#']
    pMoves = many1' $ choice [Forward <$> decimal, Turn L <$ char 'L', Turn R <$ char 'R']
    entries ls = [(V2 x y, fromJust s) | (x, l) <- zip [0 ..] ls, (y, s) <- zip [0 ..] l, isJust s]

step :: Board -> (Coord, Direction) -> Move -> (Coord, Direction)
step _ (c, d) (Turn t) = let turn = case t of L -> perp; R -> negate . perp in (c, turn d)
step b (c, d) (Forward n) = go n c
  where
    go k xy
        | k <= 0 = (xy, d)
        | otherwise =
            let xy' = case b M.!? (xy + d) of
                    Just Tile -> xy + d
                    Just Wall -> xy
                    Nothing ->
                        let xy'' = last . takeWhile (`M.member` b) $ iterate (subtract d) xy
                         in if b M.! xy'' == Tile then xy'' else xy
             in go (pred k) xy'

facing :: Direction -> Int
facing = \case
    V2 0 1 -> 0
    V2 1 0 -> 1
    V2 0 (-1) -> 2
    V2 (-1) 0 -> 3
    c -> error ("Bad direction: " <> show c)

part1 :: Board -> [Move] -> Int
part1 b ms =
    let start = (,V2 0 1) . fst . M.findMin $ M.filter (== Tile) b
        (V2 x y, d) = foldl' (step b) start ms
     in 1000 * succ x + 4 * succ y + facing d

main :: IO ()
main = do
    (board, moves) <- readBoardAndMoves <$> readFile "input.txt"
    print $ part1 board moves
