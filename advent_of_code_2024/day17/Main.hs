{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Attoparsec.Text
import Data.Bits (shiftR, xor)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V

data Computer = C {_regA :: Int, _regB :: Int, _regC :: Int, _ip :: Int, _ops :: Vector Int}
makeLenses ''Computer

parse_ :: Text -> Computer
parse_ = either (error "Bad parse") id . parseOnly c
  where
    c = C <$> rA <*> rB <*> rC <*> pure 0 <*> os
    rA = "Register A: " *> decimal
    rB = "\nRegister B: " *> decimal
    rC = "\nRegister C: " *> decimal
    os = V.fromList <$> ("\n\nProgram: " *> (decimal `sepBy1'` ","))

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
    go out c
        | (c ^. ip) >= (c ^. ops & V.length) - 1 = reverse out
        | otherwise =
            let
                code = (c ^. ops) V.! (c ^. ip)
                word = (c ^. ops) V.! (c ^. ip + 1)
                com = combo c word
                i = if (c ^. regA) /= 0 && code == 3 then word else c ^. ip + 2
                out' = if code == 5 then (com `mod` 8) : out else out
                c' = case code of
                    0 -> c & regA %~ (`shiftR` com)
                    1 -> c & regB %~ xor word
                    2 -> c & regB .~ com `mod` 8
                    3 -> c
                    4 -> c & regB %~ xor (c ^. regC)
                    5 -> c
                    6 -> c & regB .~ (c ^. regA) `shiftR` com
                    7 -> c & regC .~ (c ^. regA) `shiftR` com
                    _ -> error $ "Bad code: " <> show code
             in
                go out' (c' & ip .~ i)

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
