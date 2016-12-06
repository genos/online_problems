{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.FileEmbed (embedFile)
import Data.List (sortOn)

testData :: ByteString
testData = $(embedFile "test.txt")

input :: ByteString
input = $(embedFile "input.txt")

type SortedChars = [ByteString]
type Selector = SortedChars -> Char

prepare :: ByteString -> [SortedChars]
prepare = map (sortOn BC.length . BC.group . BC.sort) . BC.transpose . BC.lines

degarble :: Selector -> ByteString -> String
degarble s = map s . prepare

mostCommon :: Selector
mostCommon = BC.head . last

test1 :: Bool
test1 = "easter" == degarble mostCommon testData

part1 :: String
part1 = degarble mostCommon input

leastCommon :: Selector
leastCommon = BC.head . head

test2 :: Bool
test2 = "advent" == degarble leastCommon testData

part2 :: String
part2 = degarble leastCommon input

main :: IO ()
main = do
  print test1
  putStrLn part1
  print test2
  putStrLn part2
