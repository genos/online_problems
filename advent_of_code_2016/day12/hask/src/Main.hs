{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.FileEmbed        (embedStringFile)
import           Data.Foldable         (traverse_)
import           Data.Text             (Text, pack)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U
import           Text.Megaparsec
import           Text.Megaparsec.Lexer (integer, signed)
import           Text.Megaparsec.Text

input :: Text
input = pack $(embedStringFile "input.txt")

testInput :: Text
testInput = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"

data Register = A | B | C | D deriving (Eq,Ord,Enum)

type Value = Either Int Register

data Command
    = Cpy Value Register
    | Inc Register
    | Dec Register
    | Jnz Value Value
    deriving Eq

type Registers = U.Vector Int

type Commands = V.Vector Command

type IP = Int

register :: Parser Register
register =
  (char 'a' >> return A)
    <|> (char 'b' >> return B)
    <|> (char 'c' >> return C)
    <|> (char 'd' >> return D)

value :: Parser Value
value = eitherP (signed space $ fromInteger <$> integer) register

cpy :: Parser Command
cpy = string "cpy " >> Cpy <$> value <* space <*> register

inc :: Parser Command
inc = string "inc " >> Inc <$> register

dec :: Parser Command
dec = string "dec " >> Dec <$> register

jnz :: Parser Command
jnz = string "jnz " >> Jnz <$> value <* space <*> value

assembunny :: Parser Commands
assembunny = V.fromList <$> (cpy <|> inc <|> dec <|> jnz) `sepEndBy` newline

load :: Value -> Registers -> Int
load (Left  v) _  = v
load (Right r) rs = rs U.! fromEnum r

step :: IP -> Command -> Registers -> IP
step i (Jnz a b) rs = i + fromIntegral s
  where s = if load a rs == 0 then 1 else load b rs
step i _ _ = i + 1

update :: Registers -> Command -> Registers
update rs (Cpy a r) = rs U.// [(fromEnum r, load a rs)]
update rs (Inc r  ) = rs U.// [(i, rs U.! i + 1)] where i = fromEnum r
update rs (Dec r  ) = rs U.// [(i, rs U.! i - 1)] where i = fromEnum r
update rs (Jnz _ _) = rs

interpret :: Registers -> IP -> Commands -> Registers
interpret rs i cs | i < 0 || i >= length cs = rs
                  | otherwise               = interpret rs' i' cs
 where
  x   = cs V.! i
  i'  = step i x rs
  rs' = update rs x

run :: Registers -> Text -> Maybe Int
run rs t = load (Right A) . interpret rs 0 <$> parseMaybe assembunny t

test1 :: Bool
test1 = Just 42 == run (U.replicate 4 0) testInput

part1 :: Maybe Int
part1 = run (U.replicate 4 0) input

part2 :: Maybe Int
part2 = run (U.fromList [0, 0, 1, 0]) input

main :: IO ()
main = do
  print test1
  traverse_ print [part1, part2]
