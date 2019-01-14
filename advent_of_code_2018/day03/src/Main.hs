{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Attoparsec.Text
import           Data.Bool            (bool)
import           Data.Either          (fromRight)
import           Data.Foldable        (find, foldl')
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import qualified Data.Text.IO         as T

data Rect = R { _left   :: {-# UNPACK #-}!Word
              , _top    :: {-# UNPACK #-}!Word
              , _width  :: {-# UNPACK #-}!Word
              , _height :: {-# UNPACK #-}!Word
              }

data Claim = C { _id :: {-# UNPACK #-}!Word, _rect :: !Rect }

rect :: Parser Rect
rect = do
  _left   <- decimal
  _top    <- char ',' >> decimal
  _width  <- char ':' >> space >> decimal
  _height <- char 'x' >> decimal
  pure R { _left, _top, _width, _height }

claim :: Parser Claim
claim = do
  _id   <- char '#' >> decimal
  _rect <- space >> char '@' >> space >> rect
  pure C { _id, _rect }

covers :: Claim -> [(Word, Word)]
covers (C _ (R l t w h)) =
  [ (x, y) | x <- [l .. l + w - 1], y <- [t .. t + h - 1] ]

coverage :: [Claim] -> Map (Word, Word) Word
coverage = foldl' f Map.empty . fmap covers
  where f = foldl' (\m xy -> Map.insertWith (+) xy 1 m)

part1 :: [Claim] -> Word
part1 = sum . Map.map (bool 0 1 . (> 1)) . coverage

part2 :: [Claim] -> Maybe Word
part2 cs = _id <$> find p cs
 where
  p = all (\k -> (== 1) $ Map.findWithDefault 1 k c) . covers
  c = coverage cs

main :: IO ()
main = do
  cs <- fromRight [] . parseOnly (claim `sepBy` char '\n') <$> T.readFile
    "input"
  print $ part1 cs
  print $ part2 cs
