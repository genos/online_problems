module Isogram (isIsogram) where

import Data.Char (isAlpha, toLower)
import Data.Set  (Set, empty, insert, member)

isIsogram :: String -> Bool
isIsogram = go empty
 where
  go :: Set Char -> String -> Bool
  go _ "" = True
  go s (c:cs) | isAlpha c && member c' s = False
              | isAlpha c                = go s' cs
              | otherwise                = go s cs
   where
    c' = toLower c
    s' = insert c' s
