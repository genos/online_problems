module Main where

import Data.Attoparsec.Text
import Data.Bifunctor (second)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (sortBy)
import Data.Text (Text)
import Data.Text.IO qualified as T

parse_ :: Text -> (IntMap IntSet, [[Int]])
parse_ = either (error "Bad parse") id . parseOnly ((,) <$> rules <*> (endOfLine *> endOfLine *> pages))
  where
    rules = IM.fromListWith (<>) . fmap (second IS.singleton) <$> pairs
    pages = (decimal `sepBy1'` char ',') `sepBy1'` endOfLine
    pairs = ((,) <$> decimal <*> (char '|' *> decimal)) `sepBy1'` endOfLine

middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)

inOrder :: IntMap IntSet -> [Int] -> Bool
inOrder rs = fst . foldl' f (True, mempty)
  where
    f (ok, seen) x = (ok && IS.null (IS.intersection (IM.findWithDefault mempty x rs) seen), IS.insert x seen)

part1 :: IntMap IntSet -> [[Int]] -> Int
part1 rs = sum . fmap middle . filter (inOrder rs)

part2 :: IntMap IntSet -> [[Int]] -> Int
part2 rs = sum . fmap (middle . sortBy (⋈)) . filter (not . inOrder rs)
  where
    x ⋈ y = if IS.member y (IM.findWithDefault mempty x rs) then GT else LT

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input) . uncurry) [part1, part2]
