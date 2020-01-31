{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Lens ((&), (+~), (-~))
import Data.Foldable (foldl', traverse_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import Linear.V2 (V2 (..), _x, _y)
import Linear.Vector (zero)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Direction = U | D | L | R

pDirection :: Parser Direction
pDirection = foldl1 (<|>) $ zipWith ($>) (fmap char "UDLR") [U, D, L, R]

data Step = Step {_direction :: !Direction, _distance :: {-# UNPACK #-} !Int}

pCircuit :: Parser [Step]
pCircuit = (Step <$> pDirection <*> L.decimal) `sepBy` char ','

pCircuits :: Parser ([Step], [Step])
pCircuits = (,) <$> pCircuit <*> (char '\n' *> pCircuit)

readSteps :: IO ([Step], [Step])
readSteps = do
  raw <- T.readFile "input"
  pure $ either (error . errorBundlePretty) id $ runParser pCircuits "input" raw

step :: Direction -> V2 Int -> V2 Int
step U = (& _y +~ 1)
step D = (& _y -~ 1)
step L = (& _x -~ 1)
step R = (& _x +~ 1)

walk :: Step -> V2 Int -> [V2 Int]
walk (Step dir dist) = take (dist + 1) . iterate (step dir)

wires :: [Step] -> Set (V2 Int)
wires = snd . foldl' go (zero, mempty)
  where
    go (!xy, !xys) !s =
      let ss = walk s xy in (last ss, S.fromList (init ss) <> xys)

crossings :: ([Step], [Step]) -> Set (V2 Int)
crossings (a, b) = S.filter (/= zero) $ S.intersection (wires a) (wires b)

part1 :: ([Step], [Step]) -> Int
part1 = minimum . S.map (sum . fmap abs) . crossings

build :: [Step] -> Map (V2 Int) Int
build = fin . foldl' go (0, zero, mempty)
  where
    fin (_, _, !m) = m
    go (!i, !xy, !m) !s = (i', xy', m')
      where
        ps = zip (walk s xy) [i ..]
        (xy', i') = last ps
        m' = m <> M.fromList ps

part2 :: ([Step], [Step]) -> Int
part2 (a, b) = minimum . S.map add . S.filter (/= zero) $ S.intersection sa sb
  where
    add xy = ma M.! xy + mb M.! xy
    ma = build a
    mb = build b
    sa = M.keysSet ma
    sb = M.keysSet mb

main :: IO ()
main = do
  ab <- readSteps
  traverse_ (print . ($ ab)) [part1, part2]
