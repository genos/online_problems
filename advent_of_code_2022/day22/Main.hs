{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding (readFile)

import Data.Attoparsec.Text hiding (takeWhile)
import Data.Bifunctor (second)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Linear.V2

type Direction = V2 Int -- assumed to be ± unit vector
type Coord = V2 Int
data Turn = L | R
data Square = Tile | Wall deriving (Eq)
type Board = Map Coord Square

turn :: Turn -> Direction -> Direction
turn = \case L -> perp; R -> negate . perp

readBoardAndInstructions :: Text -> (Board, [(Int, Turn)], Int)
readBoardAndInstructions =
    either (error "Bad parse") id
        . parseOnly ((,,) <$> (M.fromList <$> pBoard) <*> (endOfLine *> endOfLine *> pInstructions) <*> decimal)
  where
    pBoard = entries <$> (pLine `sepBy1'` endOfLine)
    pLine = many1' pSquare
    pSquare = choice [Nothing <$ char ' ', Just Tile <$ char '.', Just Wall <$ char '#']
    pInstructions = many1' ((,) <$> decimal <*> choice [L <$ char 'L', R <$ char 'R'])
    entries ls = [(V2 x y, fromJust s) | (x, l) <- zip [0 ..] ls, (y, s) <- zip [0 ..] l, isJust s]

start :: Board -> (Coord, Direction)
start = (,V2 0 1) . fst . M.findMin . M.filter (== Tile)

step :: Board -> (Coord, Direction) -> (Int, Turn) -> (Coord, Direction)
step b (c, d) (n, t) = second (turn t) $ go n (c, d)
  where
    go :: Int -> (Coord, Direction) -> (Coord, Direction)
    go k (xy, dir)
        | k <= 0 = (xy, dir)
        | otherwise =
            let xy' = case b M.!? (xy + dir) of
                    Just Tile -> xy + dir
                    Just Wall -> xy
                    Nothing ->
                        let xy'' =
                                last . takeWhile (`M.member` b) $
                                    iterate (subtract dir) xy
                         in if b M.! xy'' == Tile then xy'' else xy
             in go (pred k) (xy', dir)

facing :: Direction -> Int
facing = \case V2 0 1 -> 0; V2 1 0 -> 1; V2 0 (-1) -> 2; V2 (-1) 0 -> 0; c -> error ("Bad direction: " <> show c)

part1 :: Board -> [(Int, Turn)] -> Int -> Int
part1 b is num =
    let (xy, d) = foldl' (step b) (start b) is
        (V2 x y, _) = step b (xy, d) (num, R) -- R immaterial
     in 1000 * succ x + 4 * succ y + facing d

main :: IO ()
main = do
    (board, instructions, num) <- readBoardAndInstructions <$> readFile "input.txt"
    print $ part1 board instructions num
