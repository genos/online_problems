{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (traverse_)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text.IO         as T

data SnailfishNumber = Leaf {-# UNPACK #-} !Int | Node !SnailfishNumber !SnailfishNumber
  deriving Eq

instance Show SnailfishNumber where
  show (Leaf n  ) = show n
  show (Node l r) = "[" ++ show l ++ "," ++ show r ++ "]"

readNums :: Text -> [SnailfishNumber]
readNums = fromRight (error "Bad parse") . parseOnly (num `sepBy1'` "\n")
 where
  num  = (Leaf <$> decimal) <|> node
  node = Node <$> ("[" *> num) <*> ("," *> num <* "]")

add :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
add x = reduce . Node x
  where reduce a = let b = explode a in maybe b reduce (split b)

explode :: SnailfishNumber -> SnailfishNumber
explode = fin . go (0 :: Word) Nothing
 where
  fin (_, _, x) = x
  -- with _major_ help from https://github.com/nicuveo/advent-of-code/blob/main/2021/haskell/src/Day18-2.hs
  go _ acc (Leaf n) = (Nothing, Nothing, Leaf $ n + fromMaybe 0 acc)
  go depth acc (Node l r)
    | depth < 4
    = let d               = depth + 1
          (ll, lr, left ) = go d acc l
          (rl, rr, right) = go d lr r
          newLeft         = addRight (fromMaybe 0 rl) left
      in  (ll, rr, Node newLeft right)
    | otherwise
    = (Just $ unLeaf l + fromMaybe 0 acc, Just $ unLeaf r, Leaf 0)
  addRight x = \case
    Leaf n   -> Leaf $ n + x
    Node l r -> Node l (addRight x r)
  unLeaf = \case
    Leaf n -> n
    _      -> error "unLeaf on Node"


split :: SnailfishNumber -> Maybe SnailfishNumber
split (Leaf n) | n < 10    = Nothing
               | otherwise = Just $ Node (Leaf n') (Leaf n'')
 where
  n'  = n `div` 2
  n'' = (n + 1) `div` 2
split (Node l r) = case split l of
  Just l' -> Just $ Node l' r
  Nothing -> Node l <$> split r

magnitude :: SnailfishNumber -> Int
magnitude (Leaf n  ) = n
magnitude (Node l r) = 3 * magnitude l + 2 * magnitude r

part1 :: [SnailfishNumber] -> Int
part1 = magnitude . foldl1 add

part2 :: [SnailfishNumber] -> Int
part2 sns = maximum [ magnitude $ add a b | a <- sns, b <- sns, a /= b ]

main :: IO ()
main = do
  numbers <- readNums <$> T.readFile "input.txt"
  traverse_ (print . ($ numbers)) [part1, part2]
