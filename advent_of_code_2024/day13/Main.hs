{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.IO qualified as T

data Button = B {_x :: {-# UNPACK #-} !Int, _y :: {-# UNPACK #-} !Int}
data Machine = M
    { _a :: !Button
    , _b :: !Button
    , _prizeX :: {-# UNPACK #-} !Int
    , _prizeY :: {-# UNPACK #-} !Int
    }

parse_ :: Text -> [Machine]
parse_ = either (error "Bad parse") id . parseOnly (m `sepBy1'` "\n\n")
  where
    m = M <$> b <*> ("\n" *> b) <*> ("\n" *> ("Prize: X=" *> decimal)) <*> (", Y=" *> decimal)
    b = B <$> ("Button " *> ("A" <|> "B") *> ": X+" *> decimal) <*> (", Y+" *> decimal)

{-
part1 :: [Machine] -> Int
part1 = sum . fmap minimum . filter (not . null) . fmap f
  where
    f (M a b x y) =
        [ 3 * p + q
        | p <- [0 .. 100]
        , q <- [0 .. 100]
        , x == p * _x a + q * _x b
        , y == p * _y a + q * _y b
        ]
-}

tokens :: Machine -> Int
tokens (M (B ax ay) (B bx by) x y) = if ok then 3 * i + j else 0
  where
    -- https://en.wikipedia.org/wiki/Cramer%27s_rule#Explicit_formulas_for_small_systems
    ok = (i * ax + j * bx, i * ay + j * by) == (x, y)
    i = (x * by - bx * y) `div` d
    j = (ax * y - x * ay) `div` d
    d = ax * by - bx * ay

part1, part2 :: [Machine] -> Int
part1 = sum . fmap tokens
part2 = sum . fmap (tokens . \m@(M _ _ x y) -> m{_prizeX = x + big, _prizeY = y + big})
  where
    big = 10 ^ (13 :: Int)

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
