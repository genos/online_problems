module Main
  ( main
  ) where

import Data.Attoparsec.Text
import Data.Map.Strict      ((!))
import Linear               (V2 (..), (!*), (*^))

data Action = N | S | E | W | L | R | F

actionP :: Parser Action
actionP =
  (char 'N' $> N)
    <|> (char 'S' $> S)
    <|> (char 'E' $> E)
    <|> (char 'W' $> W)
    <|> (char 'L' $> L)
    <|> (char 'R' $> R)
    <|> (char 'F' $> F)

data Instruction = Instruction
  { _action :: Action
  , _value  :: Int
  }

instructionP :: Parser Instruction
instructionP = Instruction <$> actionP <*> decimal

input :: IO [Instruction]
input =
  fromRight []
    .   parseOnly ((instructionP `sepBy1'` char '\n') <* skipSpace <* endOfInput)
    <$> readFileText "input.txt"

data Direction = North | South | East | West deriving stock (Eq, Ord)

type Point = V2 Int

dp :: [(Direction, Point)]
dp = [(North, V2 0 1), (South, V2 0 (-1)), (East, V2 1 0), (West, V2 (-1) 0)]

dirToPoint :: Direction -> Point
dirToPoint = (fromList dp !)

pointToDir :: Point -> Direction
pointToDir = ((fromList $ fmap swap dp) !)

rotate :: Direction -> Int -> Direction
rotate d = pointToDir . rotate' (dirToPoint d)

rotate' :: Point -> Int -> Point
rotate' p n = foldr (!*) p $ replicate (n `div` 90) m
  where m = V2 (V2 0 (-1)) (V2 1 0)  -- 90° clockwise rotation

run :: ((Point, a) -> Instruction -> (Point, a)) -> (Point, a) -> [Instruction] -> Int
run f p = sum . fmap abs . fst . foldl' f p

part1 :: [Instruction] -> Int
part1 = run f (V2 0 0, East)
 where
  f (p, d) (Instruction a n) = case a of
    N -> (p + n *^ dirToPoint North, d)
    S -> (p + n *^ dirToPoint South, d)
    E -> (p + n *^ dirToPoint East, d)
    W -> (p + n *^ dirToPoint West, d)
    F -> (p + n *^ dirToPoint d, d)
    L -> (p, rotate d n)
    R -> (p, rotate d $ 360 - n)

part2 :: [Instruction] -> Int
part2 = run f (V2 0 0, V2 10 1)
 where
  f (p, w) (Instruction a n) = case a of
    N -> (p, w + n *^ dirToPoint North)
    S -> (p, w + n *^ dirToPoint South)
    E -> (p, w + n *^ dirToPoint East)
    W -> (p, w + n *^ dirToPoint West)
    F -> (p + n *^ w, w)
    L -> (p, rotate' w n)
    R -> (p, rotate' w $ 360 - n)

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
