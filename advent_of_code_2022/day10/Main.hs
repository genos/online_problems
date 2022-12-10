{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (foldl', traverse_)
import Data.Text (Text)
import qualified Data.Text.IO as T

data Instruction = NOOP | ADDX {-# UNPACK #-} !Int

readInstructions :: Text -> [Instruction]
readInstructions = either (error "Bad parse") id . parseOnly (i `sepBy1'` endOfLine)
  where
    i = choice [NOOP <$ string "noop", ADDX <$> (string "addx " *> signed decimal)]

interpret :: Instruction -> [Int -> Int]
interpret = \case NOOP -> [id]; (ADDX i) -> [id, (+ i)]

part1 :: [Instruction] -> Int
part1 =  snd . foldl' f (1, 0) . zip [1 ..] . concatMap interpret
  where
    f (x, s) (i, g) = (x', s')
      where
        x' = g x
        s' = if i `elem` [20, 60, 100, 140, 180, 220] then s + (i * x) else s

main :: IO ()
main = do
    input <- readInstructions <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1]
