{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (foldl', toList, traverse_)
import Data.Functor ((<$))
import Data.List (break, sort)
import Data.Text (Text)
import qualified Data.Text.IO as T

data Entry
    = File {_size :: {-# UNPACK #-} !Int, _fname :: {-# UNPACK #-} !Text}
    | Directory {_entries :: [Entry], _dname :: {-# UNPACK #-} !Text}

-- http://learnyouahaskell.com/zippers
data Crumb = Crumb {_cname :: {-# UNPACK #-} !Text, _lefts :: [Entry], _rights :: [Entry]}
type Zipper = (Entry, [Crumb])

newRoot :: Zipper
newRoot = (Directory [] "/", [])

up :: Zipper -> Zipper
up (_, []) = error "up at root"
up (entry, (Crumb n l r) : rs) = (Directory (l ++ [entry] ++ r) n, rs)

dn :: Text -> Zipper -> Zipper
dn _ (File _ _, _) = error "dn file"
dn n (Directory es dn, cs) = (e, Crumb n ls rs : cs)
  where
    (ls, e : rs) = break eqN es
    eqN (Directory _ d) = d == n
    eqN (File _ f) = f == n

insert :: Zipper -> Entry -> Zipper
insert (File _ _, _) _ = error "insert on File focus"
insert (Directory es n, cs) e = (Directory (e : es) n, cs)

sizes :: Zipper -> [Int]
sizes = reverse . sort . annotate . toRoot
  where
    annotate (File s _) = []
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
part2 (root : rest) = minimum $ filter ((>= 30000000) . (free +)) rest
  where
    free = 70000000 - root

main :: IO ()
main = do
    tree <- readSizes <$> T.readFile "input.txt"
    traverse_ (print . ($ tree)) [part1, part2]
