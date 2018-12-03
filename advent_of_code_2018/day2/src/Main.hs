{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Monad   ((<=<))
import           Data.Bool       (bool)
import           Data.Foldable   (find, foldl')
import           Data.List       (inits)
import qualified Data.Map.Strict as M
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

part1 :: [Text] -> Int
part1 = uncurry (*) . foldl' f (0, 0)
 where
  f (!two, !three) t = (two + a, three + b)
   where
    cs = counts t
    a  = bool 0 1 $ has 2 cs
    b  = bool 0 1 $ has 3 cs
    has n = not . M.null . M.filter (== n)
    counts = T.foldl' (\m c -> M.insertWith (+) c (1 :: Word) m) M.empty

part2 :: [Text] -> Maybe Text
part2 ts = search $ zip ts $ inits ts
 where
  search  = find isProperLen <=< (find (any isProperLen) . fmap commons)
  commons (x, xs) = fmap (commonChars x) xs
  commonChars a b =
    T.filter (/= ' ') $ T.zipWith (\x y -> if x == y then x else ' ') a b
  isProperLen = (== (T.length (head ts) - 1)) . T.length

main :: IO ()
main = do
  ids <- T.lines <$> T.readFile "input"
  print . part1 $ ids
  print . part2 $ ids
