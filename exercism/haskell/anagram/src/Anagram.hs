module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter
  (\ys -> let ys' = fmap toLower ys in ys' /= xs' && sort ys' == xs'')
 where
  xs'  = fmap toLower xs
  xs'' = sort xs'
