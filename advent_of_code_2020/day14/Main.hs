{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  ) where

import Data.Attoparsec.Text
import Data.Bits            (clearBit, setBit)
import Data.IntMap.Strict   (elems)
import Data.Map.Strict      ((!?))
import Data.Vector          (Vector, ifoldM', ifoldl')

data Statement = Mask (Vector (Maybe Bool)) | Memory Int Int

maskP :: Parser Statement
maskP =
  Mask
    .   fromList
    .   reverse
    .   fmap (fromList [('0', False), ('1', True)] !?)
    <$> (string "mask = " *> count 36 (char 'X' <|> char '1' <|> char '0'))

memP :: Parser Statement
memP = Memory <$> (string "mem[" *> decimal) <*> (string "] = " *> decimal)

input :: IO [Statement]
input =
  fromRight []
    .   parseOnly (((maskP <|> memP) `sepBy1'` char '\n') <* skipSpace <* endOfInput)
    <$> readFileText "input.txt"

solve :: (Int -> Int -> Vector (Maybe Bool) -> IntMap Int) -> [Statement] -> Int
solve apply = sum . elems . snd . foldl' go (mempty, mempty)
 where
  go (mask, memory) = \case
    (Mask m    ) -> (m, memory)
    (Memory i n) -> (mask, apply i n mask <> memory)

part1 :: [Statement] -> Int
part1 = solve $ \i n mask -> one (i, ifoldl' f n mask)
 where
  f value key = \case
    Just False -> clearBit value key
    Just True  -> setBit value key
    Nothing    -> value

part2 :: [Statement] -> Int
part2 = solve $ \i n mask -> fromList ((, n) <$> ifoldM' f i mask)
 where
  f value key = \case
    Just False -> [value]
    Just True  -> [setBit value key]
    Nothing    -> [setBit value key, clearBit value key]

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
