{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text.IO as T

data SNAFU = E | M | Z | O | T deriving (Enum)

instance Show SNAFU where
    show = \case E -> "="; M -> "-"; Z -> "0"; O -> "1"; T -> "2"

s2i :: [SNAFU] -> Int
s2i = let f = subtract 2 . fromEnum in foldl' (\n d -> 5 * n + f d) 0

i2s :: Int -> [SNAFU]
i2s = go []
  where
    go ds n =
        let (q, r) = (n + 2) `divMod` 5
            ds' = toEnum r : ds
         in if q == 0 then ds' else go ds' q

readSNAFU :: Text -> [[SNAFU]]
readSNAFU = either (error "Bad parse") id . parseOnly (snafu `sepBy1'` endOfLine)
  where
    snafu = many1' $ choice [E <$ "=", M <$ "-", Z <$ "0", O <$ "1", T <$ "2"]

part1 :: [[SNAFU]] -> String
part1 = concatMap show . i2s . sum . fmap s2i

main :: IO ()
main = do
    input <- readSNAFU <$> T.readFile "input.txt"
    putStrLn $ part1 input
