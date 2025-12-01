{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.IO qualified as T

data Instruction = L Int | R Int deriving (Show)

parse_ :: Text -> [Instruction]
parse_ = either (error "Bad parse") id . parseOnly (line `sepBy1'` "\n")
  where
    line = (("L" $> L) <|> ("R" $> R)) <*> decimal

part1 :: [Instruction] -> Int
part1 = length . filter (== 0) . fmap (`mod` 100) . scanl (+) 50 . fmap (\case L n -> n; R n -> -n)

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    print $ part1 input
