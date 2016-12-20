{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad ((<$!>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.FileEmbed (embedFile)
import Data.Foldable
import Data.List (sort)
import Data.Word
import Text.Trifecta

input :: ByteString
input = $(embedFile "input.txt")

data Range =
    R {-# UNPACK #-} !Word32
      {-# UNPACK #-} !Word32
    deriving (Eq,Ord,Show)

parseRange :: Parser Range
parseRange =
    R <$!> fmap fromInteger natural <*>
    fmap fromInteger (string "-" *> natural)

parseRanges :: Parser [Range]
parseRanges = sort <$!> many parseRange  -- sort is vital

findMin :: [Range] -> Word32
findMin = foldl' go 0
  where
    go :: Word32 -> Range -> Word32
    go i (R a b)
      | i < a = i
      | otherwise = max i (b + 1)

testInput :: ByteString
testInput = BC.unlines ["5-8", "0-2", "4-7"]

test1 :: Result Bool
test1 = ((== 3) . findMin) <$> parseByteString parseRanges mempty testInput

part1 :: Result Word32
part1 = findMin <$> parseByteString parseRanges mempty input

main :: IO ()
main = do
    print test1
    print part1
