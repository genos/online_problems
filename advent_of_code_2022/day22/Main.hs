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

data Direction = N | S | E | W deriving (Enum)
type Coord = V2 Int
data RL = L | R
data Move = Forward Int | Turn RL
data Square = Tile | Wall deriving (Eq)
type Board = Map Coord Square

d2C :: Direction -> Coord
d2C = \case N -> V2 0 1; S -> V2 0 (-1); E -> V2 1 0; W -> V2 (-1) 0

turn :: RL -> Direction -> Direction
turn L N = W
turn R N = E
turn L S = E
turn R S = W
turn L E = N
turn R E = S
turn L W = S
turn R W = N

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
step _ (c, d) (Turn t) = (c, turn t d)
step b (c, d) (Forward n) = go n c
  where
    go k xy
        | k <= 0 = (xy, d)
        | otherwise =
            let d' = d2C d
                xy' = case b M.!? (xy + d') of
                    Just Tile -> xy + d'
                    Just Wall -> xy
                    Nothing ->
                        let xy'' = last . takeWhile (`M.member` b) $ iterate (subtract d') xy
                         in if b M.! xy'' == Tile then xy'' else xy
             in go (pred k) xy'

part1 :: Board -> [Move] -> Int
part1 b ms =
    let start = (,N) . fst . M.findMin $ M.filter (== Tile) b
        (V2 x y, d) = foldl' (step b) start ms
     in 1000 * succ x + 4 * succ y + succ (fromEnum d)

main :: IO ()
main = do
    (board, moves) <- readBoardAndMoves <$> readFile "input.txt"
    print $ part1 board moves
