{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

parse_ :: Text -> (Set Text, [Text])
parse_ = either (error "Bad parse") id . parseOnly ((,) <$> available <*> ("\n\n" *> designs))
  where
    available = S.fromList <$> takeWhile1 isColor `sepBy1'` ", "
    designs = takeWhile1 isColor `sepBy1'` "\n"
    isColor c = c == 'w' || c == 'u' || c == 'b' || c == 'r' || c == 'g'

stripPrefixes :: Set Text -> Text -> Set Text
stripPrefixes available design = foldl' f S.empty available
  where
    f ds x = maybe ds (`S.insert` ds) $ T.stripPrefix x design

possible :: Set Text -> Text -> Bool
possible available design
    | T.null design = True
    | otherwise = any (possible available) $ stripPrefixes available design

part1 :: Set Text -> [Text] -> Int
part1 available = length . filter (possible available)

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    print . uncurry part1 $ input
