{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Text.Trifecta
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.FileEmbed (embedFile)

input :: ByteString
input = $(embedFile "input.txt")

data Input
    = Plain ByteString
    | Compressed { _reps :: Int
                 , _string :: ByteString}

aXb :: Parser (Int, Int)
aXb =
    parens $
    do a <- natural
       _ <- char 'x'
       b <- natural
       return (fromIntegral a, fromIntegral b)

compressed1 :: Parser Int
compressed1 = do
    (c,r) <- aXb
    s <- count c anyChar
    return $ r * length s

plain :: Parser Int
plain = length <$> some (notChar '(')

parse :: Parser Int -> Parser Int -> ByteString -> [Int]
parse f g b =
    case parseByteString (many $ f <|> g) mempty b of
        Success is -> is
        Failure ei -> fail . show $ ei

decompress1 :: ByteString -> Int
decompress1 = sum . parse compressed1 plain

test1in :: [ByteString]
test1in =
    [ "ADVENT"
    , "A(1x5)BC"
    , "(3x3)XYZ"
    , "A(2x2)BCD(2x2)EFG"
    , "(6x1)(1x3)A"
    , "X(8x2)(3x3)ABCY"]

test1out :: [ByteString]
test1out =
    [ "ADVENT"
    , "ABBBBBC"
    , "XYZXYZXYZ"
    , "ABCBCDEFEFG"
    , "(1x3)A"
    , "X(3x3)ABC(3x3)ABCY"]

test1 :: Bool
test1 = and $ zipWith (==) (map decompress1 test1in) (map BC.length test1out)

part1 :: Int
part1 = decompress1 input

compressed2 :: Parser Int
compressed2 = do
    (c,r) <- aXb
    s <- count c anyChar
    return $ r * (sum . parse compressed2 plain $ BC.pack s)

decompress2 :: ByteString -> Int
decompress2 = sum . parse compressed2 plain

test2in :: [ByteString]
test2in =
    [ "(3x3)XYZ"
    , "X(8x2)(3x3)ABCY"
    , "(27x12)(20x12)(13x14)(7x10)(1x12)A"
    , "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"]

test2out :: [Int]
test2out =
    [BC.length "XYZXYZXYZ", BC.length "XABCABCABCABCABCABCY", 241920, 445]

test2 :: Bool
test2 = and $ zipWith (==) (map decompress2 test2in) test2out

part2 :: Int
part2 = decompress2 input

main :: IO ()
main = do
    print test1
    print part1
    print test2
    print part2
