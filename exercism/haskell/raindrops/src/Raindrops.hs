module Raindrops (convert) where

import Data.Bool (bool)

convert :: Int -> String
convert n = bool s (show n) (null s)
 where
  s = concatMap (\(k, p) -> bool "" p (0 == n `mod` k))
                [(3, "Pling"), (5, "Plang"), (7, "Plong")]
