{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.Foldable              (foldl')
import           Data.Functor               (($>))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
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
  (char 'U' $> U) <|> (char 'D' $> D) <|> (char 'L' $> L) <|> (char 'R' $> R)

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

type XY = (Int, Int)

step :: Direction -> XY -> XY
step U (x, y) = (x, y + 1)
step D (x, y) = (x, y - 1)
step L (x, y) = (x - 1, y)
step R (x, y) = (x + 1, y)

walk :: XY -> Step -> [XY]
walk xy (Step dir dist) = take (dist + 1) $ iterate (step dir) xy

wires :: [Step] -> Set XY
wires = snd . foldl' go ((0, 0), mempty)
 where
  go (!xy, !xys) !s =
    let ss = walk xy s in (last ss, S.fromList (init ss) <> xys)

crossings :: ([Step], [Step]) -> Set XY
crossings (a, b) = S.filter (/= (0, 0)) $ S.intersection (wires a) (wires b)

part1 :: ([Step], [Step]) -> Int
part1 = minimum . S.map taxi . crossings where taxi (x, y) = abs x + abs y

build :: [Step] -> Map XY Int
build = fin . foldl' go (0, (0, 0), mempty)
 where
  fin (_, _, !m) = m
  go (!i, !xy, !m) !s = (i', xy', m')
   where
    ps        = zip (walk xy s) [i ..]
    (xy', i') = last ps
    m'        = m <> M.fromList ps

part2 :: ([Step], [Step]) -> Int
part2 (a, b) = minimum . S.map add . S.filter (/= (0, 0)) $ S.intersection sa
                                                                           sb
 where
  add xy = ma M.! xy + mb M.! xy
  ma = build a
  mb = build b
  sa = M.keysSet ma
  sb = M.keysSet mb

main :: IO ()
main = do
  ab <- readSteps
  print $ part1 ab
  print $ part2 ab
