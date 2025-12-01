module Main (main) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.IO qualified as T

parse_ :: Text -> [Int]
parse_ = either (error "Bad parse") id . parseOnly (line `sepBy1'` char '\n')
  where
    line = ((char 'L' $> id) <|> (char 'R' $> negate)) <*> decimal

part1 :: [Int] -> Int
part1 = length . filter (== 0) . fmap (`mod` 100) . scanl (+) 50

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    print $ part1 input
