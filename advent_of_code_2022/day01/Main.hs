{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text hiding (take)
import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text.IO as T

parseNums :: Text -> [Int]
parseNums = either (error "Bad parse") (fmap sum) . parseOnly (many' (decimal `sepBy1'` "\n" <* skipSpace))

main :: IO ()
main = do
  nums <- parseNums <$> T.readFile "input.txt"
  traverse_ (print . ($ nums)) [maximum, sum . take 3 . reverse . sort]
