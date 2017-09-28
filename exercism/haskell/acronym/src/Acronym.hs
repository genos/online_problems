module Acronym (abbreviate) where

import Data.Char (isPunctuation, isUpper, toUpper)

abbreviate :: String -> String
abbreviate = concatMap handle . words . punct
 where
  handle, punct :: String -> String
  handle s | all isUpper s = [head s]
           | otherwise     = filter isUpper (toUpper (head s) : tail s)
  punct = fmap (\c -> if isPunctuation c then ' ' else c)
