{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Lens ((+=), (-=), Lens', _Just, assign, makeLenses, use, view)
import Control.Monad.Extra ((&&^), whenM)
import Control.Monad.Trans.State (execState, State)
import Data.FileEmbed (embedStringFile)
import Data.Text (Text, pack)
import Data.Vector ((!), Vector, fromList)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text (Parser)
import Text.Megaparsec.Lexer (integer, signed)

input :: Text
input = pack $(embedStringFile "input.txt")

testInput :: Text
testInput = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"

type Value = Either Int Char

data Command = Cpy Value Char
             | Inc Char
             | Dec Char
             | Jnz Value Int

data Simulator = Sim { _a    :: Int
                     , _b    :: Int
                     , _c    :: Int
                     , _d    :: Int
                     , _ip   :: Int
                     , _tape :: Vector Command }
makeLenses ''Simulator

bunny :: Parser (Vector Command)
bunny = fromList <$> (cpy <|> inc <|> dec <|> jnz) `sepEndBy` newline
  where
    cpy = string "cpy " >> Cpy <$> val <* space <*> letterChar
    inc = string "inc " >> Inc <$> letterChar
    dec = string "dec " >> Dec <$> letterChar
    jnz = string "jnz " >> Jnz <$> val <* space <*> int
    val = eitherP int letterChar
    int = signed space $ fromInteger <$> integer

reg :: Char -> Lens' Simulator Int
reg 'a' = a
reg 'b' = b
reg 'c' = c
reg 'd' = d
reg x   = error $ "Unknown register: " ++ show x

load :: Value -> State Simulator Int
load (Left i)  = return i
load (Right r) = use $ reg r

interpret :: Command -> State Simulator ()
interpret (Cpy v r) = load v >>= assign (reg r)
interpret (Inc r)   = reg r += 1
interpret (Dec r)   = reg r -= 1
interpret (Jnz v o) = whenM ((/= 0) <$> load v) $ ip += o - 1

evaluate :: State Simulator ()
evaluate = do
  line <- use ip
  whenM (return (line >= 0) &&^ ((line <) . length <$> use tape)) $ do
    use tape >>= interpret . (! line)
    ip += 1
    evaluate

part1 :: Text -> Int
part1 =
  view a . execState evaluate . Sim 0 0 0 0 0 . view _Just . parseMaybe bunny

test1 :: Bool
test1 = 42 == part1 testInput

part2 :: Text -> Int
part2 =
  view a . execState evaluate . Sim 0 0 1 0 0 . view _Just . parseMaybe bunny

main :: IO ()
main = do
  print test1
  print $ part1 input
  print $ part2 input
