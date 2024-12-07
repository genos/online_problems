{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Word (Word64)

type Equation = (Word64, [Word64])
type Op = Word64 -> Word64 -> Word64

parse_ :: Text -> [Equation]
parse_ = either (error "Bad parse") id . parseOnly (equation `sepBy1'` "\n")
  where
    equation = (,) <$> (decimal <* ": ") <*> (decimal `sepBy1'` " ")

results :: [Op] -> [Word64] -> [Word64]
results _ [] = error "empty list"
results ops (x : xs) = go x xs <$> replicateM (length xs) ops
  where
    go a [] _ = a
    go _ _ [] = error "shouldn't happen"
    go a (y : ys) (f : fs) = go (f a y) ys fs

solve :: [Op] -> [Equation] -> Word64
solve ops = sum . fmap fst . filter (\(t, r) -> t `elem` results ops r)

part1 :: [Equation] -> Word64
part1 = solve [(+), (*)]

part2 :: [Equation] -> Word64
part2 = solve [(+), (*), conc]
  where
    conc x y = x * 10 ^ nDigits y + y
    -- copy-pasta from number-length package
    nDigits :: Word64 -> Word64
    nDigits n
        | n < 10 = 1
        | n < 100 = 2
        | n < 1000 = 3
        | n < 10000 = 4
        | n >= 10000000000000000 = 16 + nDigits (n `div` 10000000000000000)
        | n >= 100000000 = 8 + nDigits (n `div` 100000000)
        | otherwise = 4 + nDigits (n `div` 10000)

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
