module Main where

import           Control.Arrow   ((&&&))
import           Control.Monad   ((<=<))
import           Data.Bool       (bool)
import           Data.Foldable   (find)
import           Data.List       (inits)
import           Data.Map.Strict (empty, insertWith)
import           Data.Monoid     (Sum (..))
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

part1 :: [Text] -> Int
part1 = getSum . uncurry (*) . foldMap ((has 2 &&& has 3) . counts)
 where
  has n = Sum . bool 0 1 . elem n
  counts = T.foldl' (\m c -> insertWith (+) c (1 :: Word) m) empty

part2 :: [Text] -> Maybe Text
part2 ts = search $ zip ts $ inits ts
 where
  search = find isProperLen <=< (find (any isProperLen) . fmap commons)
  commons (x, xs) = fmap (commonChars x) xs
  commonChars a b =
    T.filter (/= ' ') $ T.zipWith (\x y -> if x == y then x else ' ') a b
  isProperLen = (== (T.length (head ts) - 1)) . T.length

main :: IO ()
main = do
  ids <- T.lines <$> T.readFile "input"
  print . part1 $ ids
  print . part2 $ ids
