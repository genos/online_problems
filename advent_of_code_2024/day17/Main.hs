{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Attoparsec.Text
import Data.Bits (xor)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

data Registers = R {_regA :: Int, _regB :: Int, _regC :: Int} deriving (Eq, Show)
type Operand = Int
data Computer = C
    { _registers :: Registers
    , _ip :: Int
    , _ops :: Vector Int
    , _output :: [Int]
    }
    deriving (Eq, Show)

makeLenses ''Registers
makeLenses ''Computer

parse_ :: Text -> Computer
parse_ = either (error "Bad parse") id . parseOnly c
  where
    c = C <$> rs <*> pure 0 <*> ws <*> pure []
    rs = R <$> ("Register A: " *> decimal) <*> ("\nRegister B: " *> decimal) <*> ("\nRegister C: " *> decimal)
    ws = V.fromList <$> ("\n\nProgram: " *> (decimal `sepBy1'` ","))

combo :: Computer -> Int -> Int
combo c =
    \case
        n | 0 <= n && n <= 3 -> n
        4 -> c ^. registers . regA
        5 -> c ^. registers . regB
        6 -> c ^. registers . regC
        n -> error $ "Bad combo value: " <> show n

next :: Computer -> Computer
next computer@(C (R a b c) i ws _)
    | i >= V.length ws - 1 = computer
    | otherwise = computer' & ip .~ i'
  where
    code = ws V.! i
    word = ws V.! (i + 1)
    com = combo computer word
    i' = if a /= 0 && code == 3 then word else i + 2
    computer' = case code of
        0 -> computer & (registers . regA) .~ a `div` 2 ^ com
        1 -> computer & (registers . regB) .~ xor b word
        2 -> computer & (registers . regB) .~ com `mod` 8
        3 -> computer & ip .~ i'
        4 -> computer & (registers . regB) .~ xor b c
        5 -> computer & output <|~ com `mod` 8
        6 -> computer & (registers . regB) .~ a `div` 2 ^ com
        7 -> computer & (registers . regC) .~ a `div` 2 ^ com
        _ -> error $ "Bad code: " <> show code

run :: Computer -> [Int]
run c = let c' = next c in if c' == c then c ^. output & reverse else run c'

part1 :: Computer -> Text
part1 = T.init . T.tail . T.pack . show . run

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    T.putStrLn $ part1 input