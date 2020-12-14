module Main
  ( main
  ) where

import           Data.Attoparsec.Text
import           Data.IntSet          (insert, member)
import           Data.Vector          (Vector)
import qualified Data.Vector          as V

data Operation = ACC | JMP | NOP

operation :: Parser Operation
operation = (string "acc" $> ACC) <|> (string "jmp" $> JMP) <|> (string "nop" $> NOP)

data Instruction = Instruction
  { _op  :: !Operation
  , _arg :: {-# UNPACK #-} !Int
  }

instruction :: Parser Instruction
instruction = Instruction <$> operation <* skipSpace <*> signed decimal

input :: IO (Vector Instruction)
input =
  fromList
    .   fromRight []
    .   parseOnly ((instruction `sepBy1'` char '\n') <* skipSpace <* endOfInput)
    <$> readFileText "input.txt"


data Computer = Computer
  { _accumulator :: {-# UNPACK #-} !Int
  , _seen        :: !IntSet
  , _pointer     :: {-# UNPACK #-} !Int
  }

new :: Computer
new = Computer 0 mempty 0

step :: Computer -> Instruction -> Computer
step (Computer a s p) =
  let s' = insert p s
  in  \case
        Instruction ACC n -> Computer (a + n) s' (p + 1)
        Instruction JMP n -> Computer a       s' (p + n)
        Instruction NOP _ -> Computer a       s' (p + 1)

run :: Computer -> Vector Instruction -> Either Int Int
run c@(Computer accumulator seen pointer) program
  | member pointer seen       = Left accumulator   -- loop
  | pointer >= length program = Right accumulator  -- end
  | otherwise                 = run (step c (program V.! pointer)) program

part1 :: Vector Instruction -> Int
part1 = either id (error "NO LOOP") . run new

part2 :: Vector Instruction -> Int
part2 v = V.head . V.mapMaybe (rightToMaybe . run new) $ V.generate len alter
 where
  len = V.length v
  alter k =
    let (Instruction op n) = v V.! k
        op'                = case op of
                               ACC -> ACC
                               JMP -> NOP
                               NOP -> JMP
    in  v V.// [(k, Instruction op' n)]

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
