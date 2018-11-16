module Main where

import Data.Foldable (traverse_)
import Data.Maybe (fromJust)

mad :: [Integer] -> [Integer] -> Maybe Integer
mad [] _ = Nothing
mad _ [] = Nothing
mad (x:xs) (y:ys) = Just $ m xs ys (abs $ x - y)
    where
        m [] _ d = d
        m _ [] d = d
        m (a:as) (b:bs) d | d' == 1   = 1
                          | a < b     = m as (b:bs) d'
                          | otherwise = m (a:as) bs d'
                          where
                              d' = min d (abs $ a - b)


main :: IO ()
main = traverse_ (print . uncurry mad) [([], [1..]),
                                    ([1..], []),
                                    ([19, 22, 24], [37, 28, 49, 88])]
