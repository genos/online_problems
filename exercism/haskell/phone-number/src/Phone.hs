module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number = validate . filter isDigit
 where
  validate :: String -> Maybe String
  validate s | n == 10 && h /= '1' && c `elem` ['2' .. '9'] = Just s
             | n == 11 && h == '1' = validate $ tail s
             | otherwise           = Nothing
   where
    n = length s
    h = head s
    c = s !! 3
