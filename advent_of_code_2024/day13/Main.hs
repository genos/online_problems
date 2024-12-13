{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.IO qualified as T

data Button = B {_x :: {-# UNPACK #-} !Word, _y :: {-# UNPACK #-} !Word} deriving (Show)
data Machine = M
    { _a :: !Button
    , _b :: !Button
    , _prizeX :: {-# UNPACK #-} !Word
    , _prizeY :: {-# UNPACK #-} !Word
    }
    deriving (Show)

parse_ :: Text -> [Machine]
parse_ = either (error "Bad parse") id . parseOnly (m `sepBy1'` "\n\n")
  where
    m = M <$> b <*> ("\n" *> b) <*> ("\n" *> ("Prize: X=" *> decimal)) <*> (", Y=" *> decimal)
    b = B <$> ("Button " *> ("A" <|> "B") *> ": X+" *> decimal) <*> (", Y+" *> decimal)

part1 :: [Machine] -> Word
part1 = sum . fmap minimum . filter (not . null) . fmap solutions
  where
    solutions (M a b x y) = [3 * p + q | p <- [0 .. 100], q <- [0 .. 100], x == p * _x a + q * _x b, y == p * _y a + q * _y b]

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1]
