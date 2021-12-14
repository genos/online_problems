{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Bifunctor       (bimap)
import           Data.Foldable        (maximumBy, minimumBy)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Ord             (comparing)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

type Polymer = [Char]  -- aka String
type Rules = Map (Char, Char) Char
type Counter = Map Char Int

readPolymerization :: Text -> (Polymer, Rules)
readPolymerization = either (error "Bad Parse") (bimap T.unpack M.fromList)
  . parseOnly ((,) <$> template <*> (rules `sepBy1` "\n"))
 where
  template = takeTill isEndOfLine <* "\n\n"
  rules    = (,) <$> ((,) <$> letter <*> letter) <*> (" -> " *> letter)

step :: Rules -> Polymer -> Polymer
step rules ~polymer@(p : _) = (p :) . concatMap lookupAndSplice $ zip polymer $ tail
  polymer
 where
  lookupAndSplice (a, b) = case rules M.!? (a, b) of
    Nothing -> []
    Just x  -> [x, b]

toCounter :: Polymer -> Counter
toCounter = M.unionsWith (+) . fmap (`M.singleton` 1)

-- Thanks Protolude
applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)

range :: Counter -> Int
range counter = hi - lo
 where
  list = M.toList counter
  hi   = snd $ maximumBy (comparing snd) list
  lo   = snd $ minimumBy (comparing snd) list

part1 :: Rules -> Polymer -> Int
part1 rules = range . toCounter . applyN 10 (step rules)

main :: IO ()
main = do
  (polymer, rules) <- readPolymerization <$> T.readFile "input.txt"
  print $ part1 rules polymer
