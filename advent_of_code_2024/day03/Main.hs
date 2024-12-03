{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.IO qualified as T

-- https://stackoverflow.com/a/79247429
parse_ :: Parser a -> Text -> [a]
parse_ p = either (error "Bad parse") id . parseOnly (many' loop)
  where
    loop = try p <|> try (anyChar *> loop)

mul :: Parser (Int, Int)
mul = (,) <$> ("mul(" *> decimal) <*> ("," *> decimal <* ")")

solve :: ([a] -> [(Int, Int)]) -> Parser a -> Text -> Int
solve f p = sum . fmap (uncurry(*)) . f . parse_ p

part1 :: Text -> Int
part1 = solve id mul

data P2 = Do | Don't | Pair (Int, Int) deriving (Show)

p2 :: Parser P2
p2 = ("do()" $> Do) <|> ("don't()" $> Don't) <|> (Pair <$> mul)

prune :: [P2] -> [(Int, Int)]
prune = reverse . fst . foldl' f ([], True)
  where
    f (nss, _) Do = (nss, True)
    f (nss, _) Don't = (nss, False)
    f (nss, False) (Pair _) = (nss, False)
    f (nss, True) (Pair ns) = (ns : nss, True)

part2 :: Text -> Int
part2 = solve prune p2

main :: IO ()
main = do
    input <- T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
