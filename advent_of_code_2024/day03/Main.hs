{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.IO qualified as T

testInput :: Text
testInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

-- https://stackoverflow.com/a/79247429
parse_ :: Text -> [(Int, Int)]
parse_ = either (error "Bad parse") id . parseOnly (many' loop)
  where
    loop = try pairs <|> try (anyChar *> loop)
    pairs = (,) <$> ("mul(" *> decimal) <*> ("," *> decimal <* ")")

part1 :: [(Int, Int)] -> Int
part1 = sum . fmap (uncurry (*))

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1]
