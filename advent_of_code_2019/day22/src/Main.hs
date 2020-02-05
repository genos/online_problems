{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- with _lots_ of help from https://github.com/mstksg/advent-of-code-2019/blob/master/reflections.md#day-22
module Main where

import Data.Finite (Finite, getFinite, modulo)
import Data.Group (Group (invert))
import Data.Semigroup (stimes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read (decimal, signed)
import GHC.TypeNats (KnownNat)

data Affine n = Affine {_a :: !(Finite n), _b :: !(Finite n)}

instance KnownNat n => Semigroup (Affine n) where
  Affine a0 b0 <> Affine a1 b1 = Affine (a0 * a1) (a0 * b1 + b0)

instance KnownNat n => Monoid (Affine n) where
  mempty = Affine 1 0

instance KnownNat n => Group (Affine n) where
  invert (Affine a b) = let a' = a ^ (maxBound @(Finite n) - 1) in Affine a' (negate $ a' * b)

parse :: Text -> [(Integer, Integer)]
parse = fmap (convert . T.words) . reverse . T.lines
  where
    convert = \case
      "cut" : k : _ -> (1, negate $ chomp k)
      "deal" : "into" : _ -> (negate 1, negate 1)
      "deal" : "with" : _ : k : _ -> (chomp k, 0)
      s -> error $ "Unexpected input: " <> show s
    chomp = either error fst . signed decimal

run :: KnownNat n => Finite n -> (Affine n -> Affine n) -> [(Integer, Integer)] -> Integer
run i f = getFinite . (\(Affine a b) -> a * i + b) . f . foldMap (\(a, b) -> Affine (modulo a) (modulo b))

main :: IO ()
main = do
  pairs <- parse <$> T.readFile "input"
  print $ run @10007 2019 id pairs
  print . run @119315717514047 2020 (invert . stimes (101741582076661 :: Int)) $ pairs
