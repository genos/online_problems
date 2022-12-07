{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (foldl', traverse_)
import Data.List (partition, sort)
import Data.Text (Text)
import qualified Data.Text.IO as T

data Entry
    = File {_size :: {-# UNPACK #-} !Int, _fname :: {-# UNPACK #-} !Text}
    | Directory {_entries :: [Entry], _dname :: {-# UNPACK #-} !Text}  -- within a directory, names are unique

-- http://learnyouahaskell.com/zippers
data Steps = Steps {_cname :: {-# UNPACK #-} !Text, _others :: [Entry]}
type Zipper = (Entry, [Steps])

newRoot :: Zipper
newRoot = (Directory [] "/", [])

up :: Zipper -> Zipper
up (_, []) = error "up at root"
up (entry, (Steps n os) : rs) = (Directory (entry : os) n, rs)

dn :: Text -> Zipper -> Zipper
dn _ (File _ _, _) = error "dn file"
dn n (Directory es _, cs) = (e, Steps n os : cs)
  where
    (xs, os) = partition eqN es  -- within a directory, names are unique
    e = if length xs == 1 then head xs else error "conflicting names or name not found"
    eqN (Directory _ d) = d == n
    eqN (File _ f) = f == n

insert :: Zipper -> Entry -> Zipper
insert (File _ _, _) _ = error "insert on File focus"
insert (Directory es n, cs) e = (Directory (e : es) n, cs)

sizes :: Zipper -> [Int]
sizes = reverse . sort . annotate . toRoot
  where
    annotate (File _ _) = []
    annotate d@(Directory es _) = size d : concatMap annotate es
    toRoot z@(e, cs) = if null cs then e else toRoot $ up z
    size (File s _) = s
    size (Directory es _) = sum (size <$> es)

readSizes :: Text -> [Int]
readSizes = either (error "Bad parse") sizes . parseOnly (parseZipper =<< newRoot <$ (string "$ cd /" <* endOfLine))
  where
    parseZipper z = (choice [contents z, step z] <* endOfLine) >>= \z' -> choice [z' <$ endOfInput, parseZipper z']
    contents z = foldl' insert z <$> ((string "$ ls" *> endOfLine) *> (entry `sepBy'` endOfLine))
    step z = choice [up z <$ string "$ cd ..", (`dn` z) <$> (string "$ cd " *> name)]
    entry = choice [Directory [] <$> (string "dir " *> name), File <$> decimal <*> (skipSpace *> name)]
    name = takeTill isEndOfLine

part1 :: [Int] -> Int
part1 = sum . filter (<= 100000) . tail

part2 :: [Int] -> Int
part2 [] = error "empty list"
part2 (root : rest) = minimum $ filter ((>= 30000000) . (free +)) rest
  where
    free = 70000000 - root

main :: IO ()
main = do
    tree <- readSizes <$> T.readFile "input.txt"
    traverse_ (print . ($ tree)) [part1, part2]
