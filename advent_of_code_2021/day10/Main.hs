module Main where

import Data.Foldable (foldl', traverse_)
import Data.List     (sort)
import Data.Maybe    (mapMaybe)

data OpenClose = Open | Close deriving Eq
data Item = Paren | Bracket | Brace | Angle deriving (Eq, Enum)

cToPair :: Char -> (OpenClose, Item)
cToPair '(' = (Open, Paren)
cToPair ')' = (Close, Paren)
cToPair '[' = (Open, Bracket)
cToPair ']' = (Close, Bracket)
cToPair '{' = (Open, Brace)
cToPair '}' = (Close, Brace)
cToPair '<' = (Open, Angle)
cToPair '>' = (Close, Angle)
cToPair c   = error $ "Unexpected Character: " ++ show c

solve
  :: ([Int] -> Int)  -- score
  -> Maybe Int  -- complete
  -> ([Item] -> Maybe Int)  -- incomplete
  -> (Item -> Maybe Int)  -- corrupted
  -> String  -- input
  -> Int  -- output
solve score complete incomplete corrupted =
  score . mapMaybe (go [] . fmap cToPair) . lines
 where
  go []    []               = complete
  go opens []               = incomplete opens
  go []    ((Close, x) : _) = corrupted x
  go (x : opens) ((Close, y) : pairs) | x == y    = go opens pairs
                                      | otherwise = corrupted y
  go opens ((Open, x) : pairs) = go (x : opens) pairs

part1 :: String -> Int
part1 = solve sum Nothing (const Nothing) (Just . toInt)
  where toInt = ([3, 57, 1197, 25137] !!) . fromEnum

part2 :: String -> Int
part2 = solve (median . sort) Nothing (Just . toInt) (const Nothing)
 where
  median []     = error "Empty list in `median`"
  median [n]    = n
  median [_, _] = error "They promised us only lists of odd length!"
  median ns     = median . tail $ init ns
  toInt = foldl' (\n item -> 5 * n + 1 + fromEnum item) 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  traverse_ (print . ($ input)) [part1, part2]
