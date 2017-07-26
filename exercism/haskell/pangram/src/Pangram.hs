module Pangram (isPangram) where

import Data.Char (toLower)
import Data.Set  (fromList, isSubsetOf)

isPangram :: String -> Bool
isPangram text = chars `isSubsetOf` text'
 where
  chars = fromList ['a' .. 'z']
  text' = fromList . fmap toLower $ text
