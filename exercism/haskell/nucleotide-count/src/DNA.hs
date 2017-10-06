module DNA (nucleotideCounts) where

import           Data.Foldable   (foldl')
import           Data.Map.Strict (Map, fromList, insertWith, member)
import qualified Data.Map.Strict as M

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts = foldl' f (Right empty)
 where
  empty :: Map Char Int
  empty = fromList [('A', 0), ('C', 0), ('G', 0), ('T', 0)]
  f :: Either String (Map Char Int) -> Char -> Either String (Map Char Int)
  f (Right m) c | c `member` m = Right $ insertWith (+) c 1 m
  f _         c                = Left $ "Bad nucleotide character: " ++ [c]
