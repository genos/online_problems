{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.FileEmbed (embedFile)
import Text.Trifecta

input :: ByteString
input = $(embedFile "input.txt")

testInput :: ByteString
testInput =
    BC.unlines ["cpy 41 a", "inc a", "inc a", "dec a", "jnz a 2", "dec a"]

data Register = A | B | C | D deriving Eq

instance Show Register where
    show A = "a"
    show B = "b"
    show C = "c"
    show D = "d"

data IorR = I Integer | R Register deriving Eq

instance Show IorR where
    show (I i) = show i
    show (R r) = show r

data Command = Cpy IorR Register | Inc Register | Dec Register | Jnz IorR IorR
    deriving Eq

instance Show Command where
    show (Cpy a r) = "cpy " ++ show a ++ " " ++ show r
    show (Inc r) = "inc " ++ show r
    show (Dec r) = "dec " ++ show r
    show (Jnz a b) = "jnz " ++ show a ++ " " ++ show b

register :: Parser Register
register =
    (char 'a' >> return A) <|> (char 'b' >> return B) <|>
    (char 'c' >> return C) <|>
    (char 'd' >> return D)

iOrR :: Parser IorR
iOrR = (I <$> integer) <|> (R <$> register)

cpy :: Parser Command
cpy = do
    _ <- text "cpy" <* spaces
    a <- iOrR <* spaces
    b <- register
    return $! Cpy a b

inc :: Parser Command
inc = do
    _ <- text "inc" <* spaces
    r <- register
    return $! Inc r

dec :: Parser Command
dec = do
    _ <- text "dec" <* spaces
    r <- register
    return $! Dec r

jnz :: Parser Command
jnz = do
    _ <- text "jnz" <* spaces
    a <- iOrR <* spaces
    b <- iOrR <* spaces
    return $! Jnz a b

assembunny :: Parser [Command]
assembunny = (cpy <|> inc <|> dec <|> jnz) `sepBy` newline

main :: IO ()
main = print . BC.length $ input
