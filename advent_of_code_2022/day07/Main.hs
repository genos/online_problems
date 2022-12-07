{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (find, foldl')
import Data.Functor ((<$))
import Data.List (break, sort)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Tree (Tree (..), flatten)

data Entry
    = File {_size :: {-# UNPACK #-} !Int, _fname :: {-# UNPACK #-} !Text}
    | Directory {_entries :: [Entry], _dname :: {-# UNPACK #-} !Text}
    deriving (Show)

-- http://learnyouahaskell.com/zippers
data Crumb = Crumb {_cname :: {-# UNPACK #-} !Text, _lefts :: [Entry], _rights :: [Entry]} deriving (Show)
type Zipper = (Entry, [Crumb])

newRoot :: Zipper
newRoot = (Directory [] "/", [])

toTree :: Zipper -> Tree Int
toTree = annotate . toRoot
  where
    annotate (File s _) = Node 0 []
    annotate (Directory es _) = let s = sum (size <$> es) in Node s (fmap annotate es)
    toRoot (e, []) = e
    toRoot z = toRoot $ up z
    size (File s _) = s
    size (Directory es _) = sum (size <$> es)

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

parseRoot :: Parser Zipper
parseRoot = newRoot <$ (string "$ cd /" <* endOfLine)

parseZipper :: Zipper -> Parser Zipper
parseZipper z = do
    z' <- choice [contents z, step z] <* endOfLine
    choice [z' <$ endOfInput, parseZipper z']
  where
    name = takeTill isEndOfLine
    step z = choice [up z <$ string "$ cd ..", (`dn` z) <$> (string "$ cd " *> name)]
    contents z = foldl' insert z <$> ((string "$ ls" *> endOfLine) *> (entry `sepBy'` endOfLine))
    entry = choice [Directory [] <$> (string "dir " *> name), File <$> decimal <*> (skipSpace *> name)]

readZipper :: Text -> Zipper
readZipper = either (error "Bad parse") id . parseOnly (parseZipper =<< parseRoot)

part1 :: Tree Int -> Int
part1 = getSum . foldMap (\i -> if i > 100000 then 0 else Sum i)

part2 :: Tree Int -> Maybe Int
part2 t = let (root:rest) = flatten t
              free = 70000000 - root
           in find (\n -> free + n >= 30000000) $ sort rest

main :: IO ()
main = do
    tree <- toTree . readZipper <$> T.readFile "input.txt"
    print $ part1 tree
    print $ part2 tree
