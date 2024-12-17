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

data Computer = C {_regA :: Int, _regB :: Int, _regC :: Int, _ip :: Int, _ops :: Vector Int}
makeLenses ''Computer

parse_ :: Text -> Computer
parse_ = either (error "Bad parse") id . parseOnly c
  where
    c = C <$> rA <*> rB <*> rC <*> pure 0 <*> ws
    rA = "Register A: " *> decimal
    rB = "\nRegister B: " *> decimal
    rC = "\nRegister C: " *> decimal
    ws = V.fromList <$> ("\n\nProgram: " *> (decimal `sepBy1'` ","))

combo :: Computer -> Int -> Int
combo c =
    \case
        n | 0 <= n && n <= 3 -> n
        4 -> c ^. regA
        5 -> c ^. regB
        6 -> c ^. regC
        n -> error $ "Bad combo value: " <> show n

run :: Computer -> [Int]
run = go []
  where
    go output computer@(C a b c i ws)
        | i >= V.length ws - 1 = reverse output
        | otherwise =
            let
                code = ws V.! i
                word = ws V.! (i + 1)
                com = combo computer word
                i' = if a /= 0 && code == 3 then word else i + 2
                output' = if code == 5 then (com `mod` 8) : output else output
                computer' = case code of
                    0 -> computer & regA .~ a `div` 2 ^ com
                    1 -> computer & regB .~ xor b word
                    2 -> computer & regB .~ com `mod` 8
                    3 -> computer & ip .~ i'
                    4 -> computer & regB .~ xor b c
                    5 -> computer
                    6 -> computer & regB .~ a `div` 2 ^ com
                    7 -> computer & regC .~ a `div` 2 ^ com
                    _ -> error $ "Bad code: " <> show code
             in
                go output' (computer' & ip .~ i')

part1 :: Computer -> Text
part1 = T.init . T.tail . T.pack . show . run

part2 :: Computer -> Maybe Int
part2 c = V.find quine $ V.enumFromTo 0 maxBound
  where
    quine a = run (c & regA .~ a) == (c ^. ops & V.toList)

main :: IO ()
main = do
    input <- parse_ <$> T.readFile "input.txt"
    T.putStrLn $ part1 input
    print $ part2 input
