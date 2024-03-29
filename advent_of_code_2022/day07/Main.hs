{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (foldl', traverse_)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T

data Entry
    = File {_size :: {-# UNPACK #-} !Int, _fname :: {-# UNPACK #-} !Text}
    | Directory {_entries :: Set Entry, _dname :: {-# UNPACK #-} !Text}
    deriving (Eq, Ord)

-- http://learnyouahaskell.com/zippers
data Steps = Steps {_cname :: {-# UNPACK #-} !Text, _others :: Set Entry}
type Zipper = (Entry, [Steps])

up :: Zipper -> Zipper
up (_, []) = error "up at root"
up (entry, (Steps n os) : rs) = (Directory (S.insert entry os) n, rs)

dn :: Text -> Zipper -> Zipper
dn _ (File _ _, _) = error "dn file"
dn n (Directory es _, cs) = (e, Steps n os : cs)
  where
    (xs, os) = S.partition (\case (Directory _ d) -> d == n; _ -> False) es -- within a directory, names are unique
    e = if length xs == 1 then S.findMin xs else error "conflicting names or name not found"

insert :: Zipper -> Entry -> Zipper
insert (File _ _, _) _ = error "insert on File focus"
insert (Directory es n, cs) e = (Directory (S.insert e es) n, cs)

sizes :: Zipper -> [Int]
sizes = reverse . sort . annotate . toRoot
  where
    annotate (File _ _) = []
    annotate d@(Directory es _) = size d : concatMap annotate es
    toRoot z@(e, cs) = if null cs then e else toRoot $ up z
    size (File s _) = s
    size (Directory es _) = sum $ S.map size es

readSizes :: Text -> [Int]
readSizes = either (error "Bad parse") sizes . parseOnly (zipper =<< root <$ ("$ cd /" <* endOfLine))
  where
    root = (Directory S.empty "/", [])
    zipper z = (choice [contents z, step z] <* endOfLine) >>= \z' -> choice [z' <$ endOfInput, zipper z']
    contents z = foldl' insert z <$> (("$ ls" *> endOfLine) *> (entry `sepBy'` endOfLine))
    step z = choice [up z <$ "$ cd ..", (`dn` z) <$> ("$ cd " *> name)]
    entry = choice [Directory S.empty <$> ("dir " *> name), File <$> decimal <*> (skipSpace *> name)]
    name = takeTill isEndOfLine

part1 :: [Int] -> Int
part1 = sum . filter (<= 100000) . tail

part2 :: [Int] -> Int
part2 [] = error "empty list"
part2 (root : rest) = let free = 70000000 - root in minimum $ filter ((>= 30000000) . (free +)) rest

main :: IO ()
main = do
    tree <- readSizes <$> T.readFile "input.txt"
    traverse_ (print . ($ tree)) [part1, part2]
