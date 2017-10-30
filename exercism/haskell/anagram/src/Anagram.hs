module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (\ys -> f ys /= f xs && g ys == g xs)
 where
  f = fmap toLower
  g = sort . f
