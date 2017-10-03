module Hamming (distance) where

import Data.Align      (alignWith)
import Data.Semigroup  (Sum (..))
import Data.These      (These (..))
import Numeric.Natural (Natural)

distance :: String -> String -> Maybe Int
distance x y = fmap (length . filter id) . sequenceA $ alignWith f x y
 where
  f :: These Char Char -> Maybe Bool
  f (These a b) = Just $ a /= b
  f _           = Nothing

distance' :: String -> String -> Maybe Natural
distance' x y = fmap (getSum . mconcat) . sequenceA $ alignWith f x y
 where
  f :: These Char Char -> Maybe (Sum Natural)
  f (These a b) = Just . Sum $ if a == b then 0 else 1
  f _           = Nothing

distanceNaive :: String -> String -> Maybe Int
distanceNaive x y
  | length x /= length y = Nothing
  | otherwise            = Just . length . filter id $ zipWith (/=) x y

distanceNaive' :: String -> String -> Maybe Natural
distanceNaive' "" "" = Just 0
distanceNaive' _  "" = Nothing
distanceNaive' "" _  = Nothing
distanceNaive' (x:xs) (y:ys) | x == y    = d
                             | otherwise = (1+) <$> d
  where d = distanceNaive' xs ys
