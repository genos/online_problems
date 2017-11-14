module Series (largestProduct) where

import Data.Char (digitToInt, isNumber)
import Data.List (tails)

largestProduct :: Int -> String -> Maybe Integer
largestProduct size digits | size > length digits        = Nothing
                           | size < 0                    = Nothing
                           | any (not . isNumber) digits = Nothing
                           | otherwise                   = Just $ maximum prods
 where
  nums    = fmap (fromIntegral . digitToInt) digits
  windows = filter ((size==) . length) . fmap (take size) $ tails nums
  prods   = fmap product windows
