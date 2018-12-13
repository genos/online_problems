{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow        ((&&&))
import qualified Data.Attoparsec.Text as P
import           Data.Function        (on)
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
  I.fromList . filter alive . uncurry spread $! (I.findMin &&& I.findMax) pots
 where
  alive i = neighbors i `S.member` rules
  neighbors i =
    I.fromList . fmap (+ (2 - i)) . filter (`I.member` pots) $! spread i i

sum' :: IntSet -> Int
sum' = I.foldl' (+) 0

part1 :: Pots -> Set Rule -> Int
part1 pots rules = sum' $ go 20 pots
 where
  go :: Int -> Pots -> Pots
  go !n !ps | n <= 0    = ps
            | otherwise = go (n - 1) (step rules ps)

findStability :: Pots -> Set Rule -> (Integer, Integer, Integer)
findStability pots rules = go 0 pots (s pots) (s $! s pots)
 where
  s = step rules
  go :: Int -> Pots -> Pots -> Pots -> (Integer, Integer, Integer)
  go !n !ps !qs !rs | d == d'   = (f $! n, f $! d, f $! sum' ps)
                    | otherwise = go (n + 1) qs rs (s rs)
   where
    !d  = diff qs ps
    !d' = diff rs qs
    diff = (-) `on` sum'
    f    = fromIntegral

part2 :: Pots -> Set Rule -> Integer
part2 pots rules = (full - steps) * diff + current
 where
  full                   = 50000000000
  (steps, diff, current) = findStability pots rules

main :: IO ()
main = do
  (pots, rules) <- parseInput
  print $ part1 pots rules
  print $ part2 pots rules
