{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Attoparsec.Text
import           Data.Bool            (bool)
import           Data.Either          (fromRight)
import           Data.Foldable        (find, foldl')
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import qualified Data.Text.IO         as T

data Claim = Claim { elfId      :: {-# UNPACK #-}!Word
                   , leftBorder :: {-# UNPACK #-}!Word
                   , topBorder  :: {-# UNPACK #-}!Word
                   , width      :: {-# UNPACK #-}!Word
                   , height     :: {-# UNPACK #-}!Word
                   } deriving Show

claim :: Parser Claim
claim = do
  _          <- char '#'
  elfId      <- decimal
  _          <- space
  _          <- char '@'
  _          <- space
  leftBorder <- decimal
  _          <- char ','
  topBorder  <- decimal
  _          <- char ':'
  _          <- space
  width      <- decimal
  _          <- char 'x'
  height     <- decimal
  pure Claim {elfId , leftBorder , topBorder , width , height }

readClaims :: IO [Claim]
readClaims =
  (fromRight [] . parseOnly (claim `sepBy` char '\n')) <$> T.readFile "input"

covers :: Claim -> [(Word, Word)]
covers (Claim _ l t w h) =
  [ (x, y) | x <- [l .. l + w - 1], y <- [t .. t + h - 1] ]

coverage :: [Claim] -> Map (Word, Word) Word
coverage = foldl' cover Map.empty
 where
  cover fabric =
    foldl' (\m (x, y) -> Map.insertWith (+) (x, y) 1 m) fabric . covers

part1 :: [Claim] -> Word
part1 = Map.foldl' (+) 0 . Map.map (bool 0 1 . (> 1)) . coverage

part2 :: [Claim] -> Maybe Word
part2 claims = elfId <$> find solo claims
  where
    solo = all p . covers
    cs = coverage claims
    p k = (== 1) $ Map.findWithDefault 1 k cs

main :: IO ()
main = do
  claims <- readClaims
  print $ part1 claims
  print $ part2 claims
