{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Tuple (swap)

test :: Text
test =
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
    \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
    \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
    \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
    \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

data Color = Red | Green | Blue deriving (Eq, Ord, Show)
data Game = Game {num :: Int, subsets :: [Map Color Int]} deriving (Show)

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

main :: IO ()
main = do
    print . part1 $ readGames test
    input <- T.readFile "input.txt"
    print . part1 $ readGames input
