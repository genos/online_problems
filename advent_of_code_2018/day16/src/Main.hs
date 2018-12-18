{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Attoparsec.Text as P
import           Data.Bits            ((.&.), (.|.))
import           Data.Bool            (bool)
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

data Opcode = ADDR | ADDI
            | MULR | MULI
            | BANR | BANI
            | BORR | BORI
            | SETR | SETI
            | GTIR | GTRI | GTRR
            | EQIR | EQRI | EQRR
            deriving (Eq, Ord, Bounded, Enum, Show)

data Instruction o = I { _opcode :: !o
                       , _a      :: {-# UNPACK #-}!Word
                       , _b      :: {-# UNPACK #-}!Word
                       , _c      :: {-# UNPACK #-}!Word
                       } deriving (Eq, Show)

eval :: Instruction Opcode -> Registers -> Registers
eval (I ADDR a b c) r = set r c $ r @. a + r @. b
eval (I ADDI a b c) r = set r c $ r @. a + b
eval (I MULR a b c) r = set r c $ r @. a * r @. b
eval (I MULI a b c) r = set r c $ r @. a * b
eval (I BANR a b c) r = set r c $ r @. a .&. r @. b
eval (I BANI a b c) r = set r c $ r @. a .&. b
eval (I BORR a b c) r = set r c $ r @. a .|. r @. b
eval (I BORI a b c) r = set r c $ r @. a .|. b
eval (I SETR a _ c) r = set r c $ r @. a
eval (I SETI a _ c) r = set r c a
eval (I GTIR a b c) r = set r c $ bool 0 1 (a > r @. b)
eval (I GTRI a b c) r = set r c $ bool 0 1 (r @. a > b)
eval (I GTRR a b c) r = set r c $ bool 0 1 (r @. a > r @. b)
eval (I EQIR a b c) r = set r c $ bool 0 1 (a == r @. b)
eval (I EQRI a b c) r = set r c $ bool 0 1 (r @. a == b)
eval (I EQRR a b c) r = set r c $ bool 0 1 (r @. a == r @. b)

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

validOps :: Sample -> [Opcode]
validOps (S input instr output) = filter matches [minBound .. maxBound]
  where matches opcode = eval (instr { _opcode = opcode }) input == output

part1 :: [Sample] -> Int
part1 = length . filter ((>= 3) . length . validOps)

data Program = P { _instructions :: !(Instruction Word)
                 , _opcodes      :: !(IntMap Opcode)
                 }

main :: IO ()
main = do
  [p1, _p2] <- T.splitOn "\n\n\n" <$> T.readFile "input"
  let samples = either error id (P.parseOnly part1P p1)
  print $ part1 samples
