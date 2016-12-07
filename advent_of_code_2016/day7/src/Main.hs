{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.FileEmbed (embedFile)

input :: ByteString
input = $(embedFile "input.txt")

data IPpart = Super { _s :: ByteString } | Hyper { _h :: ByteString } deriving Show
data IPv7 = IPv7 {_parts :: [IPpart]} deriving Show

parseSuper :: AC.Parser IPpart
parseSuper = Super . BC.pack <$> AC.many1' AC.letter_ascii

parseHyper :: AC.Parser IPpart
parseHyper =
  Hyper . BC.pack <$>
    (AC.char '[' *> AC.many1' AC.letter_ascii <* AC.char ']')

parseIPpart :: AC.Parser IPpart
parseIPpart = parseHyper <|> parseSuper

parseIPv7 :: AC.Parser IPv7
parseIPv7 = IPv7 <$> AC.many' parseIPpart

isSuper :: IPpart -> Bool
isSuper (Super _) = True
isSuper _ = False

isHyper :: IPpart -> Bool
isHyper (Hyper _) = True
isHyper _ = False

supers :: IPv7 -> [ByteString]
supers = map _s . filter isSuper . _parts

hypernets :: IPv7 -> [ByteString]
hypernets = map _h . filter isHyper . _parts

substrings :: Int -> ByteString -> [ByteString]
substrings n = filter ((== n) . BC.length) . map (BC.take n) . BC.tails

hasABBA :: ByteString -> Bool
hasABBA = any f . substrings 4 where
  f :: ByteString -> Bool
  f s | BC.length s /= 4              = False
      | a == a' && b == b' && a /= b  = True
      | otherwise                     = False
    where
      a = BC.index s 0
      b = BC.index s 1
      b' = BC.index s 2
      a' = BC.index s 3

tls :: IPv7 -> Bool
tls i = any hasABBA (supers i) && all (not . hasABBA) (hypernets i)

test1 :: Bool
test1 = a && b && c && d
  where
    a = true . fmap tls $ AC.parseOnly parseIPv7 "abba[mnop]qst"
    b = true . fmap (not . tls) $ AC.parseOnly parseIPv7 "abcd[bddb]xyyx"
    c = true . fmap (not . tls) $ AC.parseOnly parseIPv7 "aaaa[qwer]tyui"
    d = true . fmap tls $ AC.parseOnly parseIPv7 "ioxxoj[asdfgh]zxcbvn"
    true = either (const False) id

part1 :: Int
part1 = either error id $ fmap countTLS ips where
  countTLS = length . filter tls
  ips = AC.parseOnly (AC.many' $ parseIPv7 <* AC.endOfLine) input

ssl :: IPv7 -> Bool
ssl = const True

test2 :: Bool
test2 = a && b && c && d
  where
    a = true . fmap ssl $ AC.parseOnly parseIPv7 "aba[bab]xyz"
    b = true . fmap (not . ssl) $ AC.parseOnly parseIPv7 "xyx[xyx]xyx"
    c = true . fmap ssl $ AC.parseOnly parseIPv7 "aaa[kek]eke"
    d = true . fmap ssl $ AC.parseOnly parseIPv7 "zazbz[bzb]cdb"
    true = either (const False) id

main :: IO ()
main = do
  print test1
  print part1
