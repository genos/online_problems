module Triangle (rows) where

import           Data.Foldable (toList)
import           Data.List     (unfoldr)
import qualified Data.Sequence as S  -- `containers` package

rows :: Int -> [[Integer]]
rows n = take n $ [1] : unfoldr f (S.singleton 1)
  where
    f :: S.Seq Integer -> Maybe ([Integer], S.Seq Integer)
    f xs =
      let ys = S.zipWith (+) (0 S.<| xs) (xs S.|> 0) in Just (toList ys, ys)
