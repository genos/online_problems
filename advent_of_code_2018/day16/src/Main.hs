{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Attoparsec.Text as P
import           Data.Bits
import           Data.IntMap.Strict   (IntMap)
import qualified Data.IntMap.Strict   as I
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

data Registers = R { _r0 :: {-# UNPACK #-}!Word
                   , _r1 :: {-# UNPACK #-}!Word
                   , _r2 :: {-# UNPACK #-}!Word
                   , _r3 :: {-# UNPACK #-}!Word
                   } deriving (Eq, Show)

registersP :: P.Parser Registers
registersP = do
  _r0 <- P.char '[' *> P.decimal <* P.char ',' <* P.skipSpace
  _r1 <- P.decimal <* P.char ',' <* P.skipSpace
  _r2 <- P.decimal <* P.char ',' <* P.skipSpace
  _r3 <- P.decimal <* P.char ']'
  pure R {_r0 , _r1 , _r2 , _r3 }

(@.) :: Registers -> Word -> Word
r @. 0 = _r0 r
r @. 1 = _r1 r
r @. 2 = _r2 r
r @. 3 = _r3 r
_ @. _ = error "out of bounds"

set :: Registers -> Word -> Word -> Registers
set r 0 v = r { _r0 = v }
set r 1 v = r { _r1 = v }
set r 2 v = r { _r2 = v }
set r 3 v = r { _r3 = v }
set _ _ _ = error "out of bounds"

setB :: Registers -> Word -> Bool -> Registers
setB r 0 b = r { _r0 = if b then 1 else 0 }
setB r 1 b = r { _r1 = if b then 1 else 0 }
setB r 2 b = r { _r2 = if b then 1 else 0 }
setB r 3 b = r { _r3 = if b then 1 else 0 }
setB _ _ _ = error "out of bounds"

data Opcode = Addr | Addi
            | Mulr | Muli
            | Banr | Bani
            | Borr | Bori
            | Setr | Seti
            | Gtir | Gtri | Gtrr
            | Eqir | Eqri | Eqrr
            deriving (Eq, Ord, Bounded, Enum, Show)

allOpcodes :: [Opcode]
allOpcodes = enumFromTo minBound maxBound

data Instruction o = I { _opcode :: !o
                       , _a      :: {-# UNPACK #-}!Word
                       , _b      :: {-# UNPACK #-}!Word
                       , _c      :: {-# UNPACK #-}!Word
                       } deriving (Eq, Show)

eval :: Instruction Opcode -> Registers -> Registers
eval (I Addr a b c) r = set r c $ r @. a + r @. b
eval (I Addi a b c) r = set r c $ r @. a + b
eval (I Mulr a b c) r = set r c $ r @. a * r @. b
eval (I Muli a b c) r = set r c $ r @. a * b
eval (I Banr a b c) r = set r c $ r @. a .&. r @. b
eval (I Bani a b c) r = set r c $ r @. a .&. b
eval (I Borr a b c) r = set r c $ r @. a .|. r @. b
eval (I Bori a b c) r = set r c $ r @. a .|. b
eval (I Setr a _ c) r = set r c $ r @. a
eval (I Seti a _ c) r = set r c a
eval (I Gtir a b c) r = setB r c $ a > r @. b
eval (I Gtri a b c) r = setB r c $ r @. a > b
eval (I Gtrr a b c) r = setB r c $ r @. a > r @. b
eval (I Eqir a b c) r = setB r c $ a == r @. b
eval (I Eqri a b c) r = setB r c $ r @. a == b
eval (I Eqrr a b c) r = setB r c $ r @. a == r @. b

data Sample = S { _input  :: !Registers
                , _instr  :: !(Instruction Word)
                , _output :: !Registers
                } deriving (Eq, Show)

instructionP :: P.Parser (Instruction Word)
instructionP = do
  [op, a, b, c] <- P.count 4 $ P.skipSpace *> P.decimal
  pure (I op a b c)

experimentP :: P.Parser Sample
experimentP = do
  _input  <- P.string "Before:" *> P.skipSpace *> registersP <* P.endOfLine
  _instr  <- instructionP <* P.endOfLine
  _output <- P.string "After:" *> P.skipSpace *> registersP
  pure S {_input , _instr , _output }

part1P :: P.Parser [Sample]
part1P = experimentP `P.sepBy1'` P.string "\n\n"

numMatches :: Sample -> Int
numMatches (S input instr output) = length $ filter matches allOpcodes
  where matches opcode = eval (instr { _opcode = opcode }) input == output

part1 :: [Sample] -> Int
part1 = length . filter ((>= 3) . numMatches)

data Program = P { _instructions :: !(Instruction Word)
                 , _opcodes :: !(IntMap Opcode)
                 }

main :: IO ()
main = do
  [p1, _p2] <- T.splitOn "\n\n\n" <$> T.readFile "input"
  let samples = either error id (P.parseOnly part1P p1)
  print $ part1 samples
