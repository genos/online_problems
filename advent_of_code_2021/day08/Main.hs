{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text hiding (D)
import           Data.Bool            (bool)
import           Data.Functor         (($>))
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Data.Vector.Sized    (Vector)
import qualified Data.Vector.Sized    as V

data DigitChar = A | B | C | D | E | F | G deriving (Enum, Eq, Ord)

instance Show DigitChar where
  show = (: []) . toEnum . (+ fromEnum 'a') . fromEnum

data Entry = Entry
  { signalPatterns :: Vector 10 [DigitChar]
  , outputValue    :: Vector 4 [DigitChar]
  }
  deriving Show

readEntries :: Text -> [Entry]
readEntries = either (error "Bad parse") id . parseOnly (many1' entry)
 where
  entry = Entry <$> sp <*> ("| " *> ov)
  sp    = err "signal patterns" . V.fromList <$> count 10 (many1' dc <* skipSpace)
  ov    = err "output value" . V.fromList <$> count 4 (many1' dc <* skipSpace)
  dc    = choice ["a" $> A, "b" $> B, "c" $> C, "d" $> D, "e" $> E, "f" $> F, "g" $> G]
  err   = fromMaybe . error

part1 :: [Entry] -> Int
part1 = sum . fmap (V.sum . V.map (bool 0 1 . isUnique) . outputValue)
  where isUnique = (`elem` [2, 3, 4, 7]) . length

main :: IO ()
main = do
  entries <- readEntries <$> T.readFile "input.txt"
  print $ part1 entries
