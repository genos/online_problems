{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Attoparsec.Text as P
import           Data.IntSet          (IntSet)
import qualified Data.IntSet          as I
import           Data.Ix              (range)
import           Data.Maybe           (catMaybes)
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

type Rule  = IntSet  -- only keep those with alive outputs
type State = IntSet  -- only keep alive positions

isPot :: Char -> Bool
isPot = (||) <$> (== '#') <*> (== '.')

readAlive :: Text -> IntSet
readAlive = I.fromList . fmap fst . filter ((== '#') . snd) . zip [0 ..] . T.unpack

ruleP :: P.Parser (Maybe Rule)
ruleP = do
  left  <- P.take 5
  right <- P.string " => " *> P.satisfy isPot
  pure $! if right == '.' then Nothing else Just $! readAlive left

stateP :: P.Parser State
stateP = do
  _      <- P.string "initial state: "
  plants <- P.takeWhile isPot
  pure $! readAlive plants

inputP :: P.Parser (State, Set Rule)
inputP = do
  state <- stateP <* P.skipSpace
  rules <- (ruleP `P.sepBy` P.char '\n') <* P.char '\n'
  pure (state, S.fromList $! catMaybes rules)

parseInput :: IO (State, Set Rule)
parseInput = do
  input <- T.readFile "input"
  either error pure $! P.parseOnly inputP input

step :: Set Rule -> State -> State
step rules state =
  I.fromList . filter alive $ range (I.findMin state - 5, I.findMax state + 5)
    where
      alive i     = (neighbors i) `S.member` rules
      neighbors i = _todo -- bad: I.fromList $ range (i - 2, i + 2)

main :: IO ()
main = parseInput >>= print
