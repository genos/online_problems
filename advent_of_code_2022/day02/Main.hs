{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Attoparsec.Text hiding (D)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text.IO as T

data LDW = L | D | W deriving (Eq, Ord, Enum)
data RPS = R | P | S deriving (Eq, Ord, Enum)
data ABC = A | B | C
data XYZ = X | Y | Z

parseInput :: Text -> [(ABC, XYZ)]
parseInput = either (error "Bad parse") id . parseOnly (((,) <$> (abc <* skipSpace) <*> xyz) `sepBy1'` endOfLine)
  where
    abc = choice [char 'A' $> A, char 'B' $> B, char 'C' $> C]
    xyz = choice [char 'X' $> X, char 'Y' $> Y, char 'Z' $> Z]

score :: RPS -> RPS -> LDW
score them me | them == me = D
score R P = W
score P S = W
score S R = W
score _ _ = L

solve :: ((ABC, XYZ) -> (RPS, RPS)) -> [(ABC, XYZ)] -> Int
solve toRPS = sum . fmap f
  where
    f (them, me) = let (abc, xyz) = toRPS (them, me) in rps2I xyz + ldw2I (score abc xyz)
    ldw2I = (* 3) . fromEnum
    rps2I = succ . fromEnum

part1 :: [(ABC, XYZ)] -> Int
part1 = solve toRPS
  where
    toRPS (them, me) =
        let abc = (\case A -> R; B -> P; C -> S) them
            xyz = (\case X -> R; Y -> P; Z -> S) me
         in (abc, xyz)

part2 :: [(ABC, XYZ)] -> Int
part2 = solve toRPS
  where
    toRPS (them, me) =
        let abc = (\case A -> R; B -> P; C -> S) them
            xyz = g me abc
            g X = \case R -> S; P -> R; S -> P
            g Y = id
            g Z = \case R -> P; P -> S; S -> R
         in (abc, xyz)

main :: IO ()
main = do
    input <- parseInput <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
