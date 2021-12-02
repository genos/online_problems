{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl', traverse_)
import           Data.Function        ((&))
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Linear.V2

data Inst = Down Int | Up Int | Forward Int

inst :: Text -> [Inst]
inst = fromRight [] . parseOnly (p `sepBy1'` "\n")
 where
  p = choice
    [ Down <$> ("down " *> decimal)
    , Up <$> ("up " *> decimal)
    , Forward <$> ("forward " *> decimal)
    ]

part1 :: [Inst] -> Int
part1 = product . sum . fmap v2
 where
  v2 (Down    x) = V2 0 x
  v2 (Up      x) = V2 0 (-x)
  v2 (Forward x) = V2 x 0

data Sub = Sub
  { horizontal :: Int
  , depth      :: Int
  , aim        :: Int
  }

part2 :: [Inst] -> Int
part2 = ((*) <$> horizontal <*> depth) . foldl' (&) (Sub 0 0 0) . fmap go
 where
  go (Down    x) s = s { aim = aim s + x }
  go (Up      x) s = s { aim = aim s - x }
  go (Forward x) s = s { horizontal = x + horizontal s, depth = depth s + x * aim s }

main :: IO ()
main = do
  input <- inst <$> T.readFile "input.txt"
  traverse_ (print . ($ input)) [part1, part2]
