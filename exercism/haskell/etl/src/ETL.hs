{-# LANGUAGE TupleSections #-}
module ETL (transform) where

import           Data.Char       (toLower)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

transform :: Map Int String -> Map Char Int
transform = M.foldlWithKey f M.empty . M.map (fmap toLower)
 where
  f :: Map Char Int -> Int -> String -> Map Char Int
  f m i s = M.fromList ((, i) <$> s) `M.union` m
