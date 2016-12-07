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

data IPpart = Super { _s :: ByteString } | Hyper { _h :: ByteString }
data IPv7 = IPv7 {_parts :: [IPpart]}
data ABBA = ABBA Char Char
data ABA = ABA Char Char deriving Eq

parseSuper :: AC.Parser IPpart
parseSuper = Super . BC.pack <$> AC.many1' AC.letter_ascii

parseHyper :: AC.Parser IPpart
parseHyper =
  Hyper . BC.pack <$>
    (AC.char '[' *> AC.many1' AC.letter_ascii <* AC.char ']')

parseIPv7 :: AC.Parser IPv7
parseIPv7 = IPv7 <$> AC.many' (parseHyper <|> parseSuper)

supers :: IPv7 -> [ByteString]
supers = map _s . filter isSuper . _parts
  where
    isSuper (Super _) = True
    isSuper _         = False

hypers :: IPv7 -> [ByteString]
hypers = map _h . filter isHyper . _parts
  where
    isHyper (Hyper _) = True
    isHyper _ = False

subsLen :: Int -> ByteString -> [ByteString]
subsLen n = filter ((== n) . BC.length) . map (BC.take n) . BC.tails

abbas :: ByteString -> [ABBA]
abbas =  concatMap mkABBA . subsLen 4 where
  mkABBA :: ByteString -> [ABBA]
  mkABBA s | BC.length s /= 4              = []
           | a == a' && b == b' && a /= b  = [ABBA a b]
           | otherwise                     = []
    where
      a = BC.index s 0
      b = BC.index s 1
      b' = BC.index s 2
      a' = BC.index s 3

tls :: IPv7 -> Bool
tls i = (not . null $ ss) && null hs where
  ss = concatMap abbas . supers $ i
  hs = concatMap abbas . hypers $ i

test1 :: Bool
test1 = a && b && c && d
  where
    a = true . fmap tls $ AC.parseOnly parseIPv7 "abba[mnop]qst"
    b = true . fmap (not . tls) $ AC.parseOnly parseIPv7 "abcd[bddb]xyyx"
    c = true . fmap (not . tls) $ AC.parseOnly parseIPv7 "aaaa[qwer]tyui"
    d = true . fmap tls $ AC.parseOnly parseIPv7 "ioxxoj[asdfgh]zxcbvn"
    true = either (const False) id

ips :: Either String [IPv7]
ips = AC.parseOnly (AC.many' $ parseIPv7 <* AC.endOfLine) input

part1 :: Int
part1 = either error id $ fmap countTLS ips where
  countTLS = length . filter tls

invert :: ABA -> ABA
invert (ABA a b) = ABA b a

abas :: ByteString -> [ABA]
abas = concatMap mkABA . subsLen 3 where
  mkABA :: ByteString -> [ABA]
  mkABA b | BC.length b == 3 && x /= y && x == z = [ABA x y]
          | otherwise                            = []
    where
      x = BC.index b 0
      y = BC.index b 1
      z = BC.index b 2

ssl :: IPv7 -> Bool
ssl i = not (null ss) && any (`elem` hs) (map invert ss)
  where
    ss = concatMap abas . supers $ i
    hs = concatMap abas . hypers $ i

test2 :: Bool
test2 = a && b && c && d
  where
    a = true . fmap ssl $ AC.parseOnly parseIPv7 "aba[bab]xyz"
    b = true . fmap (not . ssl) $ AC.parseOnly parseIPv7 "xyx[xyx]xyx"
    c = true . fmap ssl $ AC.parseOnly parseIPv7 "aaa[kek]eke"
    d = true . fmap ssl $ AC.parseOnly parseIPv7 "zazbz[bzb]cdb"
    true = either (const False) id

part2 :: Int
part2 = either error id $ fmap countSSL ips where
  countSSL = length . filter ssl

main :: IO ()
main = do
  mapM_ print [test1, test2]
  mapM_ print [part1, part2]
