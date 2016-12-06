{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Array ((!), (//), Array, elems, listArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord, isOctDigit)
import Data.Maybe (isJust, fromJust)
import Data.Monoid ((<>), First(..))
import Crypto.Hash

input :: ByteString
input = "uqwqemis"

md5 :: ByteString -> ByteString
md5 = BC.pack . show . (hash :: ByteString -> Digest MD5)

md5Stream :: ByteString -> [ByteString]
md5Stream bs = map (md5 . BC.append bs . BC.pack . show) [0::Int ..]

fiveZeros :: ByteString -> Bool
fiveZeros = BC.all (== '0') . BC.take 5

stream1 :: ByteString -> [ByteString]
stream1 = filter fiveZeros . md5Stream

construct1 :: [ByteString] -> ByteString
construct1 = BC.pack . map (BC.head . BC.drop 5)

part1Pass :: ByteString -> ByteString
part1Pass = construct1 . take 8 . stream1

test1 :: Bool
test1 = "18f47a30" == part1Pass "abc"

part1 :: ByteString
part1 = part1Pass input

stream2 :: ByteString -> [(Int, Char)]
stream2 = map g . filter f . filter fiveZeros . md5Stream where
  f :: ByteString -> Bool
  f b = BC.length b > 7 && isOctDigit (BC.index b 5)
  g :: ByteString -> (Int, Char)
  g b = (ord c1 - ord '0', c2) where
    c1 = BC.index b 5
    c2 = BC.index b 6

construct2 :: [(Int, Char)] -> ByteString
construct2 = g . f (listArray is . repeat $ First Nothing) where
  g = BC.pack . elems . fmap (fromJust . getFirst)
  is = (0, 7)
  f :: Array Int (First Char) -> [(Int, Char)] -> Array Int (First Char)
  f cs [] = cs
  f cs ((i, e):xs) | all (isJust . getFirst) cs = cs
                   | otherwise                  = f cs' xs
    where
      cs' = cs // [(i, (cs ! i) <> (First . Just $ e))]

part2Pass :: ByteString -> ByteString
part2Pass = construct2 . stream2

test2 :: Bool
test2 = "05ace8e3" == part2Pass "abc"

part2 :: ByteString
part2 = part2Pass input

main :: IO ()
main = do
  print test1
  print test2
  BC.putStrLn part1
  BC.putStrLn part2
