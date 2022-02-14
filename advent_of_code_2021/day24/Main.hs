-- with help from https://www.reddit.com/r/adventofcode/comments/rnejv5/comment/hpsc57y
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Lens         hiding (op)
import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Functor         (($>))
import           Data.SBV
import           Data.Text            (Text)
import qualified Data.Text.IO         as T

data ALU = ALU
  { _w :: SInt64
  , _x :: SInt64
  , _y :: SInt64
  , _Z :: SInt64
  }
makeLenses ''ALU

-- Different functors used in getting & setting, so we can't just put these both under Lens' ALU SInt64 :-(
data Register = W | X | Y | Z

_get :: Register -> Getting SInt64 ALU SInt64
_get = \case
  W -> w
  X -> x
  Y -> y
  Z -> z

_set :: Register -> ASetter' ALU SInt64
_set = \case
  W -> w
  X -> x
  Y -> y
  Z -> z

type Operation = SInt64 -> SInt64 -> SInt64
data Instruction = Inp Register | Bin Operation Register (Either SInt64 Register)
type Program = [Instruction]

readProgram :: Text -> Program
readProgram = fromRight (error "Bad parse") . parseOnly (line `sepBy1'` "\n")
 where
  line = choice [Inp <$> ("inp " *> reg), Bin <$> op <*> (" " *> reg) <*> (" " *> lr)]
  op   = choice
    [ "add" $> (+)
    , "mul" $> (*)
    , "div" $> sDiv
    , "mod" $> sMod
    , "eql" $> (\a b -> oneIf $ a .== b)
    ]
  reg = choice ["w" $> W, "x" $> X, "y" $> Y, "z" $> Z]
  lr  = choice [Left . fromIntegral @Int64 <$> signed decimal, Right <$> reg]

validRun :: Program -> [SInt64] -> SBool
validRun = go (ALU 0 0 0 0)
 where
  go alu []                    []       = alu ^. z .== 0
  go alu (Inp reg       : ops) (i : is) = go (alu & _set reg .~ i) ops is
  go alu (Bin op reg lr : ops) is       = go alu' ops is
   where
    alu' = alu & _set reg .~ op a b
    a    = alu ^. _get reg
    b    = either id ((alu ^.) . _get) lr
  go _ _ _ = error "Shouldn't be reachable"

solveDay24 :: (String -> SBV Int64 -> Symbolic ()) -> Program -> IO OptimizeResult
solveDay24 opt program = optimize Lexicographic $ do
  inputs <- sInt64s [ "digit_" <> show @Int i | i <- reverse [1 .. 14] ]
  constrain $ sAll (\n -> n .>= 1 .&& n .<= 9) inputs .&& validRun program inputs
  opt "model number" $ foldl1 (\acc d -> 10 * acc + d) inputs

main :: IO ()
main = do
  program <- readProgram <$> T.readFile "input.txt"
  print =<< solveDay24 maximize program
  print =<< solveDay24 minimize program
