{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Arrow        ((&&&))
import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl', maximumBy)
import           Data.Function        (on)
import           Data.List            (sort)
import           Data.List.Split      hiding (sepBy)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust)
import           Data.Ord             (comparing)
import qualified Data.Text.IO         as T

type Minute = Word
type Count = Word
type Guard = Word

data DateTime = DT { _year   :: {-# UNPACK #-}!Word
                   , _month  :: {-# UNPACK #-}!Word
                   , _day    :: {-# UNPACK #-}!Word
                   , _hour   :: {-# UNPACK #-}!Word
                   , _minute :: {-# UNPACK #-}!Minute
                   } deriving (Eq, Ord)

data Action = BeginsShift | FallsAsleep | WakesUp deriving Eq

data Entry = E { _dateTime :: !DateTime
               , _guard    :: Maybe Guard
               , _action   :: !Action
               } deriving Eq

instance Ord Entry where
  (E dt1 _ _) <= (E dt2 _ _) = dt1 <= dt2

dateTime :: Parser DateTime
dateTime = do
  _year   <- char '[' *> decimal
  _month  <- char '-' *> decimal
  _day    <- char '-' *> decimal
  _hour   <- space *> decimal
  _minute <- char ':' *> decimal <* char ']' <* space
  pure DT {_year , _month , _day , _hour , _minute }

guard :: Parser Guard
guard = string "Guard #" *> decimal <* space

action :: Parser Action
action = choice $ zipWith (\a b -> string a >> pure b)
                          ["begins shift", "falls asleep", "wakes up"]
                          [BeginsShift, FallsAsleep, WakesUp]

entry :: Parser Entry
entry = do
  _dateTime <- dateTime
  _guard    <- option Nothing (Just <$> guard)
  _action   <- action
  pure E {_dateTime , _guard , _action }

compile :: [Entry] -> Map Guard (Map Minute Count)
compile = M.fromListWith (M.unionWith (+)) . fmap analyze . split shifts . sort
 where
  shifts = keepDelimsL . dropInitBlank . whenElt $ (== BeginsShift) . _action
  analyze = (fromJust . _guard . head) &&& (naps . tail)
  naps = foldl' (M.unionWith (+)) M.empty . fmap (tally . napped) . chunksOf 2
  tally = M.fromListWith (+) . fmap (, 1)
  napped [E f _ FallsAsleep, E w _ WakesUp] = [_minute f .. _minute w - 1]
  napped _ = []

solve :: (Map Minute Count -> Word) -> Map Guard (Map Minute Count) -> Word
solve f chart = g * m
  where
  g = fst . maxByValKey . M.map f . M.filter (not . M.null) $ chart
  m = fst . maxByValKey $ chart M.! g
  maxByValKey = maximumBy (comparing snd) . M.toList

main :: IO ()
main = do
  input <- T.readFile "input"
  let entries = fromRight [] . parseOnly (entry `sepBy` char '\n') $ input
      chart = compile entries
  print . solve sum $ chart
  print . solve maximum $ chart
