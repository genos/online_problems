{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Either                          (fromRight)
import           Data.Foldable                        (foldl', traverse_)
import           Data.Function                        ((&))
import           Data.Monoid.Action                   (Action, act)
import           Data.Monoid.SemiDirectProduct.Strict (embed, inject, untag)
import           Data.Semigroup                       (Sum (..))
import           Data.Text                            (Text)
import qualified Data.Text.IO                         as T
import           Linear.V2                            (V2 (..))

data Inst = Down Int | Up Int | Forward Int

inst :: Text -> [Inst]
inst = fromRight (error "Bad parse") . parseOnly (p `sepBy1'` "\n")
 where
  p = choice
    [ Down <$> ("down " *> decimal)
    , Up <$> ("up " *> decimal)
    , Forward <$> ("forward " *> decimal)
    ]

originalPart1 :: [Inst] -> Int
originalPart1 = product . sum . fmap v2
 where
  v2 (Down    x) = V2 0 x
  v2 (Up      x) = V2 0 (-x)
  v2 (Forward x) = V2 x 0

data Sub = Sub
  { horizontal :: Int
  , depth      :: Int
  , aim        :: Int
  }

originalPart2 :: [Inst] -> Int
originalPart2 = ((*) <$> horizontal <*> depth) . foldl' (&) (Sub 0 0 0) . fmap go
 where
  go (Down    x) s = s { aim = aim s + x }
  go (Up      x) s = s { aim = aim s - x }
  go (Forward x) s = s { horizontal = x + horizontal s, depth = depth s + x * aim s }

newtype Point = P { _unP :: V2 Int }
  deriving Semigroup via Sum (V2 Int)
  deriving Monoid via Sum (V2 Int)

solve :: Monoid m => (Int -> m) -> (Int -> m) -> (m -> V2 Int) -> [Inst] -> Int
solve downUp forward extract = product . extract . foldMap
  (\case
    Down    i -> downUp i
    Up      i -> downUp (-i)
    Forward i -> forward i
  )

toX, toY :: Int -> Point
toX = P . (`V2` 0)
toY = P . V2 0

part1 :: [Inst] -> Int
part1 = solve toY toX _unP

newtype Aim = Aim Int
  deriving Semigroup via Sum Int
  deriving Monoid via Sum Int

instance Action Aim Point where
  act (Aim a) (P (V2 h d)) = P (V2 h (d + a * h))

part2 :: [Inst] -> Int
part2 = solve (embed . Aim) (inject . toX) (_unP . untag)

main :: IO ()
main = do
  input <- inst <$> T.readFile "input.txt"
  traverse_ (print . ($ input)) [originalPart1, part1, originalPart2, part2]
