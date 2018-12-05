{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Arrow        ((&&&), second)
import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl', maximumBy)
import           Data.Function        (on)
import           Data.List            (group, groupBy, sort, sortBy)
import           Data.List.Split      hiding (sepBy)
import           Data.Monoid          (Sum(..))
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as H
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
                   } deriving (Eq, Ord, Show)

data Action = BeginsShift | FallsAsleep | WakesUp deriving (Eq, Show)

data Entry = E { _dateTime :: !DateTime
               , _guard    :: Maybe Guard
               , _action   :: !Action
               } deriving (Eq, Show)

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

type Nap = (Guard, Minute)

collect :: [Entry] -> [Nap]
collect =
  sort
  . concatMap toNap
  . fmap tally
  . tail
  . split (keepDelimsL . whenElt $ (== BeginsShift) . _action)
  . sort
    where
      toNap (g, ms) = fmap (g,) ms
      tally = (fromJust . _guard . head) &&& (concatMap napped . chunksOf 2 . tail)
      napped [E df _ FallsAsleep, E dw _ WakesUp] = [_minute df .. _minute dw]
      napped _ = []

maxByLen :: [[a]] -> [a]
maxByLen = maximumBy (comparing length)

sleptMostMinutes :: [Nap] -> (Guard, Minute)
sleptMostMinutes naps = head mostNapped
  where
    groupedNaps = groupBy ((==) `on` fst) naps
    mostNaps    = maxByLen groupedNaps
    mostNapped  = maxByLen $ groupBy ((==) `on` snd) mostNaps

part1 :: [Nap] -> Word
part1 = uncurry (*) . sleptMostMinutes

sleptMostFrequentlyOnMin :: [Nap] -> (Guard, (Minute, Int))
sleptMostFrequentlyOnMin naps = last mostFreqMinned
  where
    groupedNaps = groupBy ((==) `on` fst) naps
    guardNapMins = fmap ((fst . head) &&& (fmap snd)) groupedNaps
    mostFreqMin (g, ms) = (g, head &&& length . maxByLen . group $ ms)
    mostFreqMinned = sortBy (comparing $ snd . snd) $ fmap mostFreqMin guardNapMins

-- part2 :: [Nap] -> Word
-- part2 = uncurry (*) . sleptMostFrequentlyOnMin

compile :: [Entry] -> HashMap Guard (HashMap Minute Count)
compile =
  H.fromListWith (H.unionWith (+))
    . fmap tally
    . tail
    . split (keepDelimsL . whenElt $ (== BeginsShift) . _action)
    . sort
 where
  tally = (fromJust . _guard . head) &&& (naps . tail)
  naps  = foldl' (H.unionWith (+)) H.empty . fmap napped . chunksOf 2
  napped [E df _ FallsAsleep, E dw _ WakesUp] =
    H.fromListWith (+) . fmap (, 1) $ enumFromTo (_minute df) (_minute dw)
  napped _ = H.empty

maxByVal :: Ord b => HashMap a b -> Maybe a
maxByVal = fmap fst . maxByValKey

maxByValKey :: Ord b => HashMap a b -> Maybe (a, b)
maxByValKey m = if H.null m
  then Nothing
  else Just . maximumBy (comparing snd) . H.toList $ m

main :: IO ()
main = do
  entries <- (fromRight [] . parseOnly (entry `sepBy` char '\n'))
    <$> T.readFile "input"
  let naps = collect entries
  print . part1 $ naps
  --print . part2 $ compiled
