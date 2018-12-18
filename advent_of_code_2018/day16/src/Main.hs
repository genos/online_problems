{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow        ((&&&))
import qualified Data.Attoparsec.Text as P
import           Data.Bits            ((.&.), (.|.))
import           Data.Bool            (bool)
import           Data.IntMap.Strict   (IntMap)
import qualified Data.IntMap.Strict   as I
import           Data.Set             (Set)
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

data Registers = R { _r0 :: {-# UNPACK #-}!Int
                   , _r1 :: {-# UNPACK #-}!Int
                   , _r2 :: {-# UNPACK #-}!Int
                   , _r3 :: {-# UNPACK #-}!Int
                   } deriving (Eq, Show)

registersP :: P.Parser Registers
registersP = do
  _r0 <- P.char '[' *> P.decimal <* P.char ',' <* P.skipSpace
  _r1 <- P.decimal <* P.char ',' <* P.skipSpace
  _r2 <- P.decimal <* P.char ',' <* P.skipSpace
  _r3 <- P.decimal <* P.char ']'
  pure R {_r0 , _r1 , _r2 , _r3 }

(@.) :: Registers -> Int -> Int
r @. 0 = _r0 r
r @. 1 = _r1 r
r @. 2 = _r2 r
r @. 3 = _r3 r
_ @. _ = error "out of bounds"

set :: Registers -> Int -> Int -> Registers
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
                       , _a      :: {-# UNPACK #-}!Int
                       , _b      :: {-# UNPACK #-}!Int
                       , _c      :: {-# UNPACK #-}!Int
                       } deriving (Eq, Show)

eval :: Instruction Opcode -> Registers -> Registers
eval (I o a b c) r = set r c $ case o of
  ADDR -> r @. a + r @. b
  ADDI -> r @. a + b
  MULR -> r @. a * r @. b
  MULI -> r @. a * b
  BANR -> r @. a .&. r @. b
  BANI -> r @. a .&. b
  BORR -> r @. a .|. r @. b
  BORI -> r @. a .|. b
  SETR -> r @. a
  SETI -> a
  GTIR -> bool 0 1 (a > r @. b)
  GTRI -> bool 0 1 (r @. a > b)
  GTRR -> bool 0 1 (r @. a > r @. b)
  EQIR -> bool 0 1 (a == r @. b)
  EQRI -> bool 0 1 (r @. a == b)
  EQRR -> bool 0 1 (r @. a == r @. b)

data Sample = S { _input  :: !Registers
                , _instr  :: !(Instruction Int)
                , _output :: !Registers
                } deriving (Eq, Show)

instructionP :: P.Parser (Instruction Int)
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

determineOpcodes :: [Sample] -> IntMap Opcode
determineOpcodes = go I.empty . I.fromList . fmap
  (_opcode . _instr &&& S.fromDistinctAscList . validOps)
 where
  go :: IntMap Opcode -> IntMap (Set Opcode) -> IntMap Opcode
  go !done !evidence | I.null evidence = done
                     | otherwise       = _todo -- fill in singletons…

data Program = P { _instructions :: !(Instruction Int)
                 , _opcodes      :: !(IntMap Opcode)
                 }
part2P :: P.Parser [Instruction Int]
part2P = instructionP `P.sepBy1'` P.string "\n"

main :: IO ()
main = do
  [p1, p2] <- T.splitOn "\n\n\n" <$> T.readFile "input"
  let samples      = either error id (P.parseOnly part1P p1)
      instructions = either error id (P.parseOnly part2P p2)
  print $ part1 samples
  print $ length instructions
