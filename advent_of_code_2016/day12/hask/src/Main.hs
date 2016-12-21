{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed (embedStringFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

input :: Text
input = T.pack $(embedStringFile "input.txt")

testInput :: Text
testInput =
    T.unlines ["cpy 41 a", "inc a", "inc a", "dec a", "jnz a 2", "dec a"]

type Value = Int

data Register = A | B | C | D deriving (Eq,Ord,Enum)

data VorR = V Value | R Register deriving Eq

data Command = Cpy VorR Register | Inc Register | Dec Register | Jnz VorR VorR
    deriving Eq

type Registers = U.Vector Int

type Commands = V.Vector Command

type IP = Int

register :: Parser Register
register =
  (char 'a' >> return A) <|>
  (char 'b' >> return B) <|>
  (char 'c' >> return C) <|>
  (char 'd' >> return D)

signedInt :: Parser Int
signedInt = L.signed space $ fromInteger <$> L.integer

vOrR :: Parser VorR
vOrR = (V <$> signedInt) <|> (R <$> register)

cpy :: Parser Command
cpy = Cpy <$> (string "cpy" *> space *> vOrR <* space) <*> register

inc :: Parser Command
inc = Inc <$> (string "inc" *> space *> register)

dec :: Parser Command
dec = Dec <$> (string "dec" *> space *> register)

jnz :: Parser Command
jnz = Jnz <$> (string "jnz" *> space *> vOrR <* space) <*> vOrR

assembunny :: Parser Commands
assembunny = V.fromList <$> (cpy <|> inc <|> dec <|> jnz) `sepEndBy` newline

load :: VorR -> Registers -> Value
load (V v) _  = v
load (R r) rs = rs U.! fromEnum r

step :: IP -> Command -> Registers -> IP
step i (Jnz a b) rs = i + fromIntegral s
  where
    g = load a rs
    s = if g == 0 then 1 else load b rs
step i _ _ = i + 1

update :: Registers -> Command -> Registers
update rs (Cpy a r) = rs U.// [(fromEnum r, load a rs)]
update rs (Inc r)   = rs U.// [(i, 1 + rs U.! i)] where i = fromEnum r
update rs (Dec r)   = rs U.// [(i, (-1) + rs U.! i)] where i = fromEnum r
update rs (Jnz _ _) = rs

interpret :: Registers -> IP -> Commands -> Registers
interpret rs i cs
  | i < 0 || i >= fromIntegral (length cs) = rs
  | otherwise                              = interpret rs' i' cs
  where
    x = cs V.! i
    i' = step i x rs
    rs' = update rs x

run :: Registers -> Text -> Maybe Value
run rs t = load (R A) . interpret rs 0 <$> parseMaybe assembunny t

test1 :: Bool
test1 = Just 42 == run (U.replicate 4 0) testInput

part1 :: Maybe Value
part1 = run (U.replicate 4 0) input

part2 :: Maybe Value
part2 = run (U.fromList [0, 0, 1, 0]) input

main :: IO ()
main = do
    print test1
    mapM_ print [part1, part2]
