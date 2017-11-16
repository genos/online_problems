module Triangle (rows) where

import           Data.Foldable (toList)
import           Data.List     (iterate)
import qualified Data.Sequence as S  -- `containers` package

rows :: Int -> [[Integer]]
rows n = take n . fmap toList . iterate f . S.singleton $ 1
  where
    f :: S.Seq Integer -> S.Seq Integer
    f = S.zipWith (+) <$> (0 S.<|) <*> (S.|> 0)
