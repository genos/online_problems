module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Data.Bool (bool)
import Prelude   hiding (concat, filter, foldr, length, map, reverse, (++))

-- Hutton, "A tutorial on the universality and expressiveness of fold"
-- http://www.cs.nott.ac.uk/~pszgmh/fold.pdf

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x $ foldr f z xs

length :: [a] -> Int
length = foldl' (const . succ) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> bool xs (x : xs) (p x)) []

(++) :: [a] -> [a] -> [a]
(++) = flip (foldr (:))

concat :: [[a]] -> [a]
concat = foldr (++) [] -- could use `foldl'`, but left-associated (++) is O(n^2)…
