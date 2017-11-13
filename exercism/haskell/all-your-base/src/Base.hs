module Base (rebase) where

import Data.Bool     (bool)
import Data.Foldable (foldl')
import Data.List     (unfoldr)
import Data.Tuple    (swap)

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits
  | any (<=1) [inputBase, outputBase] = Nothing
  | any ((||) <$> (>=inputBase) <*> (<0)) inputDigits = Nothing
  | otherwise = Just . dig outputBase $ num inputBase inputDigits
 where
  num b = foldl' ((+) . (*b)) 0  -- Horner's rule
  dig b =
    reverse . unfoldr (\x -> bool (Just . swap $ quotRem x b) Nothing (x <= 0))
