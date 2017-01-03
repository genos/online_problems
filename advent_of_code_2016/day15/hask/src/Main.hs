{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Protolude
import Data.Char (isDigit)
import Data.FileEmbed (embedStringFile)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Read (decimal)

input :: Text
input = T.pack $(embedStringFile "input.txt")

data Disc = Disc { _n :: Int , _p :: Int , _s :: Int }

text2Disc :: Text -> Disc
text2Disc t = fromJust $ Disc <$> n <*> p <*> s
  where
    grouped = T.groupBy ((==) `on` isDigit) t
    digits = filter (T.all isDigit) grouped
    nums = map decimal digits
    ints = map (either (const 0) fst) nums
    n = ints `atMay` 0
    p = ints `atMay` 1
    s = ints `atMay` 3

testInput :: Text
testInput =
    T.unlines
        [ "Disc #1 has 5 positions; at time=0, it is at position 4."
        , "Disc #2 has 2 positions; at time=0, it is at position 1."]

-- http://stackoverflow.com/a/35529381
crt :: (Integral a, Foldable f) => f (a, a) -> (a, a)
crt = foldl' go (0, 1)
  where
    go :: (Integral a) => (a, a) -> (a, a) -> (a, a)
    go (r1, m1) (r2, m2) = (r `mod` m, m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1
    {-# INLINABLE go #-}
    -- Modular Inverse
    inv :: (Integral a) => a -> a -> a
    a `inv` m = let (_, i, _) = a `eGCD` m in i `mod` m
    {-# INLINABLE inv #-}
    -- Extended Euclidean Algorithm
    eGCD :: (Integral a) => a -> a -> (a, a, a)
    eGCD 0 b = (b, 0, 1)
    eGCD a b = let (g, s, t) = (b `mod` a) `eGCD` a
               in (g, t - (b `div` a) * s, s)
    {-# INLINABLE eGCD #-}

solve :: [Disc] -> Int
solve ds = fst $ crt remsMods
  where
    starts = map _s ds
    offsets = zipWith (-) (map negate starts) [1..]
    positions = map _p ds
    remsMods = zip offsets positions

test1 :: Bool
test1 = 5 == solve (text2Disc <$> T.lines testInput)

part1 :: Int
part1 = solve $ text2Disc <$> T.lines input

part2 :: Int
part2 = solve $ (text2Disc <$> T.lines input) ++ [Disc 7 11 0]

main :: IO ()
main = do
    print test1
    mapM_ print [part1, part2]
