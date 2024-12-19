{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.IO qualified as T

newtype Towel = Towel Text
    deriving stock (Eq, Ord)
    deriving (Show) via Text

parse_ :: Text -> (S.Set Towel, [Towel])
parse_ = either (error "Bad parse") id . parseOnly ((,) <$> available <*> ("\n\n" *> designs))
  where
    available = S.fromList . fmap Towel <$> takeWhile1 isAlpha `sepBy1'` ", "
    designs = fmap Towel <$> takeWhile1 isAlpha `sepBy1'` "\n"

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "test.txt"
    print input
