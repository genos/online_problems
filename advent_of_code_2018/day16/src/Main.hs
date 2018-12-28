{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow        ((&&&))
import           Control.Monad.State  (StateT, evalStateT, gets, lift, modify)
import qualified Data.Attoparsec.Text as P
import           Data.Bits            ((.&.), (.|.))
import           Data.Bool            (bool)
import           Data.Foldable        (foldl')
import           Data.IntMap.Strict   (IntMap)
import qualified Data.IntMap.Strict   as I
import           Data.Maybe           (fromMaybe, listToMaybe)
import           Data.Set             (Set)
import qualified Data.Set             as S
import qualified Data.Text.IO         as T

data Registers = R { _r0 :: {-# UNPACK #-}!Int
                   , _r1 :: {-# UNPACK #-}!Int
                   , _r2 :: {-# UNPACK #-}!Int
                   , _r3 :: {-# UNPACK #-}!Int
                   } deriving (Eq, Show)

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

eval :: Registers -> Instruction Opcode -> Registers
eval r (I o a b c) = set r c $ case o of
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
  GTIR -> bool 0 1 $ a > r @. b
  GTRI -> bool 0 1 $ r @. a > b
  GTRR -> bool 0 1 $ r @. a > r @. b
  EQIR -> bool 0 1 $ a == r @. b
  EQRI -> bool 0 1 $ r @. a == b
  EQRR -> bool 0 1 $ r @. a == r @. b

data Sample = S { _input  :: !Registers
                , _instr  :: !(Instruction Int)
                , _output :: !Registers
                } deriving (Eq, Show)

validOps :: Sample -> [Opcode]
validOps (S input instr output) = filter matches [minBound .. maxBound]
  where matches opcode = eval input (instr { _opcode = opcode }) == output

part1 :: [Sample] -> Int
part1 = length . filter ((>= 3) . length . validOps)

determineOpcodes :: [Sample] -> Maybe (IntMap Opcode)
determineOpcodes =
  listToMaybe . (flip evalStateT S.empty . traverse fillIn) . initialize
 where
  initialize :: [Sample] -> IntMap (Set Opcode)
  initialize =
    I.fromList . fmap (_opcode . _instr &&& S.fromDistinctAscList . validOps)
  fillIn :: Set Opcode -> StateT (Set Opcode) [] Opcode
  fillIn candidates = do
    unseen <- gets (candidates S.\\)
    pick   <- lift $ S.toList unseen
    modify $ S.insert pick
    pure pick

part2 :: [Sample] -> [Instruction Int] -> Int
part2 samples instructions = _r0 $ foldl' eval (R 0 0 0 0) steps
 where
  opcodes = fromMaybe (error "search failed") $ determineOpcodes samples
  steps   = fmap (\i -> i { _opcode = opcodes I.! _opcode i }) instructions

registersP :: P.Parser Registers
registersP = do
  _r0 <- P.char '[' *> P.decimal <* P.char ',' <* P.skipSpace
  _r1 <- P.decimal <* P.char ',' <* P.skipSpace
  _r2 <- P.decimal <* P.char ',' <* P.skipSpace
  _r3 <- P.decimal <* P.char ']'
  pure R {_r0 , _r1 , _r2 , _r3 }

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

part2P :: P.Parser [Instruction Int]
part2P = instructionP `P.sepBy1'` P.string "\n"

parser :: P.Parser ([Sample], [Instruction Int])
parser = (,) <$> part1P <*> (P.string "\n\n\n" *> part2P)

main :: IO ()
main = do
  input <- T.readFile "input"
  let (samples, instructions) = either error id (P.parseOnly parser input)
  print $ part1 samples
  print $ part2 samples instructions
