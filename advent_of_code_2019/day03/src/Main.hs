module Main where

import           Data.Foldable              (foldl', traverse_)
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text.IO               as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Direction = U | D | L | R deriving Eq

pDirection :: Parser Direction
pDirection =
  (char 'U' *> pure U)
    <|> (char 'D' *> pure D)
    <|> (char 'L' *> pure L)
    <|> (char 'R' *> pure R)

data Step = Step { _direction :: !Direction, _distance :: {-# UNPACK #-}!Int }

pStep :: Parser Step
pStep = Step <$> pDirection <*> L.decimal

pCircuit :: Parser [Step]
pCircuit = pStep `sepBy` char ','

pCircuits :: Parser ([Step], [Step])
pCircuits = (,) <$> pCircuit <*> (char '\n' *> pCircuit)

readSteps :: IO ([Step], [Step])
readSteps = do
  raw <- T.readFile "input"
  pure $ either (error . errorBundlePretty) id $ runParser pCircuits "input" raw

type Coord = (Int, Int)

step :: Direction -> Coord -> Coord
step U (x, y) = (x, y + 1)
step D (x, y) = (x, y - 1)
step L (x, y) = (x - 1, y)
step R (x, y) = (x + 1, y)

path :: (Coord, Set Coord) -> Step -> (Coord, Set Coord)
path (c, cs) (Step dir dist) = (last ss, S.fromList (init ss) <> cs)
  where ss = take (dist + 1) $ iterate (step dir) c

wires :: [Step] -> Set Coord
wires = snd . foldl' path ((0, 0), mempty)

taxi :: Coord -> Int
taxi (x, y) = abs x + abs y

crossings :: ([Step], [Step]) -> Set Coord
crossings (a, b) = S.filter (/= (0, 0)) $ S.intersection (wires a) (wires b)

part1 :: ([Step], [Step]) -> Int
part1 = minimum . S.map taxi . crossings

part2 :: ([Step], [Step]) -> Int
part2 = _todo

main :: IO ()
main = readSteps >>= \ab -> traverse_ (print . ($ ab)) [part1, part2]
