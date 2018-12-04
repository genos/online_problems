{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Arrow        ((&&&))
import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl', maximumBy)
import           Data.List            (sort)
import           Data.List.Split      hiding (sepBy)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust, fromMaybe, isJust)
import           Data.Ord             (comparing)
import qualified Data.Text.IO         as T

type Minute = Word
type Count = Word

data DateTime = DT { _year   :: {-# UNPACK #-}!Word
                   , _month  :: {-# UNPACK #-}!Word
                   , _day    :: {-# UNPACK #-}!Word
                   , _hour   :: {-# UNPACK #-}!Word
                   , _minute :: {-# UNPACK #-}!Minute
                   } deriving (Eq, Ord, Show)

newtype Guard = G { _id :: Word } deriving (Eq, Ord, Show)

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
guard = G <$> (string "Guard #" *> decimal <* space)

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
compile =
  M.fromListWith (M.unionWith (+))
    . fmap tally
    . tail
    . split (keepDelimsL . whenElt $ (== BeginsShift) . _action)
    . sort
 where
  tally = (fromJust . _guard . head) &&& (naps . tail)
  naps  = foldl' (M.unionWith (+)) M.empty . fmap napped . chunksOf 2
  napped [E df _ FallsAsleep, E dw _ WakesUp] =
    M.fromListWith (+) . fmap (, 1) $ enumFromTo (_minute df) (_minute dw)
  napped _ = M.empty

maxByVal :: Ord b => Map a b -> Maybe a
maxByVal = fmap fst . maxByValKey

maxByValKey :: Ord b => Map a b -> Maybe (a, b)
maxByValKey m = if M.null m
  then Nothing
  else Just . maximumBy (comparing snd) . M.toList $ m

part1 :: Map Guard (Map Minute Count) -> Maybe Word
part1 c = do
  g <- maxByVal . fmap sum $ c
  k <- maxByVal (c M.! g)
  pure $ _id g * fromIntegral k

part2 :: Map Guard (Map Minute Count) -> (Guard, Minute)
part2 c = _todo
  where
    maxes = fmap fromJust $ M.filter isJust $ fmap maxByValKey c

main :: IO ()
main = do
  entries <- (fromRight [] . parseOnly (entry `sepBy` char '\n'))
    <$> T.readFile "input"
  let compiled = compile entries
  print . part1 $ compiled
  --print . part2 $ compiled
