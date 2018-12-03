module Main where

import           Data.Either    (fromRight)
import           Data.Foldable  (find)
import           Data.IntSet    (empty, insert, member)
import           Data.List      (scanl')
import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T

readInt :: Text -> Int
readInt = fromRight 0 . (fmap fst . T.signed T.decimal)

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Maybe Int
part2 xs = fmap fst . find (uncurry member) $ zip list sets
 where
  list = scanl' (+) 0 . cycle $ xs
  sets = scanl' (flip insert) empty list

main :: IO ()
main = do
  xs <- fmap readInt . T.lines <$> T.readFile "input"
  print $ part1 xs
  print $ part2 xs
