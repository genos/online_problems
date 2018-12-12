{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow        ((&&&))
import qualified Data.Attoparsec.Text as P
import           Data.Foldable        (traverse_)
import           Data.IntSet          (IntSet)
import qualified Data.IntSet          as I
import           Data.Maybe           (catMaybes)
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

type Rule = IntSet  -- only keep those with alive outputs
type Pots = IntSet  -- only keep alive positions

isPot :: Char -> Bool
isPot = (||) <$> (== '#') <*> (== '.')

readAlive :: Text -> IntSet
readAlive =
  I.fromList . fmap fst . filter ((== '#') . snd) . zip [0 ..] . T.unpack

ruleP :: P.Parser (Maybe Rule)
ruleP = do
  left  <- P.take 5
  right <- P.string " => " *> P.satisfy isPot
  pure $! if right == '.' then Nothing else Just $! readAlive left

potsP :: P.Parser Pots
potsP = readAlive <$> (P.string "initial state: " *> P.takeWhile isPot)

inputP :: P.Parser (Pots, Set Rule)
inputP = do
  pots  <- potsP <* P.skipSpace
  rules <- (ruleP `P.sepBy` P.char '\n') <* P.char '\n'
  pure (pots, S.fromList $! catMaybes rules)

parseInput :: IO (Pots, Set Rule)
parseInput = do
  input <- T.readFile "input"
  either error pure $! P.parseOnly inputP input

spread :: Int -> Int -> [Int]
spread lo hi = [lo - 2 .. hi + 2]

step :: Set Rule -> Pots -> Pots
step rules pots =
  I.fromList . filter alive . uncurry spread $ (I.findMin &&& I.findMax) pots
 where
  alive i = neighbors i `S.member` rules
  neighbors i =
    I.fromList . fmap ((+ 2) . subtract i) . filter (`I.member` pots) $ spread
      i
      i

run :: Int -> Pots -> Set Rule -> Int
run n pots rules = I.foldl' (+) 0 . (!! n) $ iterate (step rules) pots

part1 :: Pots -> Set Rule -> Int
part1 = run 20

part2 :: Pots -> Set Rule -> Int
part2 = run 50000000000

main :: IO ()
main = do
  (pots, rules) <- parseInput
  traverse_ (print . (\p -> p pots rules)) [part1, part2]
