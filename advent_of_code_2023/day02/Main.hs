{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.IO qualified as T

data Subset = S {red :: Word, green :: Word, blue :: Word} deriving (Eq)
instance Semigroup Subset where
    (S r1 g1 b1) <> (S r2 g2 b2) = S (r1 `max` r2) (g1 `max` g2) (b1 `max` b2)
instance Monoid Subset where
    mempty = S 0 0 0

type Game = (Word, [Subset])

readGames :: Text -> [Game]
readGames = either (error "Bad parse") id . parseOnly ((game `sepBy1'` "\n") <* endOfInput)
  where
    r = (\n -> S n 0 0) <$> decimal <* " red"
    g = (\n -> S 0 n 0) <$> decimal <* " green"
    b = S 0 0 <$> decimal <* " blue"
    entry = r <|> g <|> b
    subset = mconcat <$> entry `sepBy1'` ", "
    game = (,) <$> ("Game " *> decimal <* ": ") <*> (subset `sepBy1'` "; ")

part1 :: [Game] -> Word
part1 = sum . fmap fst . filter possible
  where
    bag = S 12 13 14
    possible = (== bag) . mconcat . (bag :) . snd

part2 :: [Game] -> Word
part2 = sum . fmap power
  where
    power = (\(S r g b) -> r * g * b) . mconcat . snd

main :: IO ()
main = do
    input <- readGames <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
