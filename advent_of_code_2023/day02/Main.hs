{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Tuple (swap)

data Color = Red | Green | Blue deriving (Eq, Ord)
data Game = Game {num :: Int, subsets :: [Map Color Int]}

readGames :: Text -> [Game]
readGames = either (error "Bad parse") id . parseOnly ((game `sepBy1'` "\n") <* endOfInput)
  where
    color = ("red" $> Red) <|> ("green" $> Green) <|> ("blue" $> Blue)
    entry = fmap swap . (,) <$> decimal <*> (" " *> color)
    subset = M.fromList <$> (entry `sepBy1'` ", ")
    game = Game <$> ("Game " *> decimal <* ": ") <*> (subset `sepBy1'` "; ")

part1 :: [Game] -> Int
part1 = sum . fmap num . filter possible
  where
    bag = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]
    possible = all ((== bag) . M.unionWith max bag) . subsets

part2 :: [Game] -> Int
part2 = sum . fmap power
  where
    power = product . M.unionsWith max . subsets

main :: IO ()
main = do
    input <- readGames <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
