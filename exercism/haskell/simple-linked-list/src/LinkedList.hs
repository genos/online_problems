module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

import qualified Data.Foldable as F

data LinkedList a = Empty | Pair a (LinkedList a) deriving (Eq, Show)

instance F.Foldable LinkedList where
  foldr f z Empty      = z
  foldr f z (Pair x l) = foldr f (f x z) l

datum :: LinkedList a -> a
datum Empty      = error "`Empty` has no `datum`"
datum (Pair x l) = x

assemble :: Foldable f => f a -> LinkedList a
assemble = F.foldr' Pair Empty

fromList :: [a] -> LinkedList a
fromList = assemble

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _     = False

new :: a -> LinkedList a -> LinkedList a
new = Pair

next :: LinkedList a -> LinkedList a
next Empty      = error "`Empty` has no `next`"
next (Pair _ l) = l

nil :: LinkedList a
nil = Empty

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = assemble

toList :: LinkedList a -> [a]
toList = F.foldl' (flip (:)) [] -- or `reverse . F.toList`
