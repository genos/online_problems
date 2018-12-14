{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Arrow   ((&&&), (***))
import           Control.Lens    (makeLenses, over, view, (^.))
import           Data.Either     (partitionEithers)
import           Data.Finite     (Finite)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (catMaybes)
import           Data.Ord        (comparing)
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Linear.V2       (V2 (..), _x, _y)


type Counter = Finite 3

type XY = V2 Int

newtype YX = YX { _getXY :: XY } deriving (Eq, Show)

-- compare by second coord, then first
instance Ord YX where
  compare = comparing (view _y . _getXY) <> comparing (view _x . _getXY)

data Direction = N | S | E | W deriving (Eq, Show)

data Event a = Crash XY a | Step a | Done XY deriving Functor

data Turn = NE  -- @\\@
          | NW  -- @/@
          | I   -- @+@
          deriving (Eq, Show)

data Cart = C { _turnCount :: !Counter , _direction :: !Direction } deriving (Eq, Show)
makeLenses ''Cart

type Turns = Map XY Turn

type Carts = Map YX Cart

parseWorld :: Text -> (Turns, Carts)
parseWorld t = mconcat *** mconcat $! partitionEithers ps
 where
  ls       = T.lines t
  (xs, ys) = (length &&& (maximum . fmap T.length)) ls
  ixs      = [ (x, y) | x <- [0 .. xs], y <- [0 .. ys] ]
  ps       = catMaybes . fmap readPoint . zip ixs $! T.unpack t

readPoint :: ((Int, Int), Char) -> Maybe (Either Turns Carts)
readPoint ((x, y), c) =
  let p = V2 x y
      q = YX p
  in  case c of
        '\\' -> Just . Left $! M.singleton p NE
        '/'  -> Just . Left $! M.singleton p NW
        '+'  -> Just . Left $! M.singleton p I
        '^'  -> Just . Right $! M.singleton q (C 0 N)
        'v'  -> Just . Right $! M.singleton q (C 0 S)
        '>'  -> Just . Right $! M.singleton q (C 0 E)
        '<'  -> Just . Right $! M.singleton q (C 0 W)
        _    -> Nothing

main :: IO ()
main = (print . parseWorld) =<< T.readFile "input"
