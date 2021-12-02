{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl', traverse_)
import           Data.Function        ((&))
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Linear.V2

newLines :: Parser a -> Text -> [a]
newLines p = fromRight [] . parseOnly (p `sepBy1'` "\n")

part1 :: Text -> Int
part1 = product . sum . newLines inst
 where
  inst = choice
    [ V2 0 <$> ("down " *> decimal)
    , negate . V2 0 <$> ("up " *> decimal)
    , flip V2 0 <$> ("forward " *> decimal)
    ]

data Pos = Pos
  { horizontal :: {-# UNPACK #-} !Int
  , depth      :: {-# UNPACK #-} !Int
  , aim        :: {-# UNPACK #-} !Int
  }

part2 :: Text -> Int
part2 = ((*) <$> horizontal <*> depth) . foldl' (&) (Pos 0 0 0) . newLines inst
 where
  inst = choice
    [ d <$> ("down " *> decimal)
    , u <$> ("up " *> decimal)
    , f <$> ("forward " *> decimal)
    ]
  d x p = p { aim = aim p + x }
  u x p = p { aim = aim p - x }
  f x p = p { horizontal = x + horizontal p, depth = depth p + x * aim p }

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  traverse_ (print . ($ input)) [part1, part2]
