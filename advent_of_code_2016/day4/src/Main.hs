{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Arrow ((&&&))
import Data.ByteString (ByteString)
import Data.Char (chr, isDigit, isPunctuation, ord)
import qualified Data.ByteString.Char8 as BC
import Data.FileEmbed (embedFile)
import Data.List (sortBy)
import Data.Maybe (fromJust)

data Room = Room
    { _name :: ByteString
    , _sectorID :: Int
    , _checkSum :: ByteString
    } deriving (Eq, Show)

mkRoom :: ByteString -> Room
mkRoom bs = Room n s c where
  n = BC.init . fst $ split1
  s = fst . fromJust . BC.readInt . fst $ split2
  c = BC.filter (not . isPunctuation) . snd $ split2
  split1 = BC.span (not . isDigit) bs
  split2 = BC.span (not . isPunctuation) . snd $ split1

myOrder :: (Int, Char) -> (Int, Char) -> Ordering
(i1, c1) `myOrder` (i2, c2) | i1 == i2 && c1 == c2 = EQ
                            | i1 < i2              = GT
                            | i1 == i2 && c1 < c2  = LT
                            | otherwise            = LT

isValidRoom :: Room -> Bool
isValidRoom r = c  == _checkSum r where
  g = BC.group . BC.sort . BC.filter (not . isPunctuation) . _name $ r
  m = map (BC.length &&& BC.head) g
  s = sortBy myOrder m
  c = BC.pack . take 5 . map snd $ s

input :: ByteString
input = $(embedFile "input.txt")

rooms :: [Room]
rooms = filter isValidRoom . map mkRoom . BC.lines $ input

decrypt :: Int -> ByteString -> ByteString
decrypt n = BC.map d where
  d '-' = ' '
  d c = chr $ oa + (((oc - oa) + n') `mod` 26) where
    oa = ord 'a'
    oc = ord c
    n' = n `mod` 26

decryptRoom :: Room -> ByteString
decryptRoom r = decrypt (_sectorID r) (_name r)

isNorthPoleRoom :: Room -> Bool
isNorthPoleRoom =
  (not . BC.null) . snd . BC.breakSubstring "northpole" . decryptRoom

part1 :: Int
part1 = sum . map _sectorID $ rooms

part2 :: Int
part2 = _sectorID . head . filter isNorthPoleRoom $ rooms

main :: IO ()
main = mapM_ print [part1, part2]
