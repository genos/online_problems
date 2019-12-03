module Main where

import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

rd :: Text -> Integer
rd = fromRight 0 . (fmap fst . T.signed T.decimal)

weight :: Integer -> Integer
weight = (`max` 0) . subtract 2 . (`div` 3)

part1 :: [Integer] -> Integer
part1 = sum . fmap weight

fullWeight :: Integer -> Integer
fullWeight = sum . tail . takeWhile (> 0) . iterate weight

part2 :: [Integer] -> Integer
part2 = sum . fmap fullWeight

main :: IO ()
main = do
  xs <- fmap rd . T.lines <$> T.readFile "input"
  print $ part1 xs
  print $ part2 xs
