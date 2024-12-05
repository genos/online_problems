module Main where

import Data.Attoparsec.Text
import Data.Bifunctor (second)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as U

type Vec = Vector Int
type Rules = IntMap IntSet

parse_ :: Text -> (Rules, [Vec])
parse_ = either (error "Bad parse") id . parseOnly ((,) <$> rules <*> (endOfLine *> endOfLine *> pages))
  where
    rules = IM.fromListWith (<>) . fmap (second IS.singleton) <$> pairs
    pages = fmap U.fromList <$> (decimal `sepBy1'` char ',') `sepBy1'` endOfLine
    pairs = ((,) <$> decimal <*> (char '|' *> decimal)) `sepBy1'` endOfLine

middle :: Vec -> Int
middle xs = U.unsafeIndex xs (U.length xs `div` 2)

inOrder :: Rules -> Vec -> Bool
inOrder r = fst . U.foldl' f (True, IS.empty)
  where
    f (ok, seen) x = (ok && IS.null (IS.intersection (IM.findWithDefault IS.empty x r) seen), IS.insert x seen)

part1 :: Rules -> [Vec] -> Int
part1 r = sum . fmap middle . filter (inOrder r)

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [uncurry part1]
