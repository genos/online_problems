{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile, take)

import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.SBV
import Data.Text (Text)
import Data.Text.IO (readFile)

data Op = Plus | Minus | Mul | Div

apply :: (Num a, SDivisible a) => Op -> a -> a -> a
apply = \case
    Plus -> (+)
    Minus -> (-)
    Mul -> (*)
    Div -> sDiv

data Expr = Number Word64 | Operation Text Op Text

readMonkeys :: Text -> Map Text Expr
readMonkeys = either (error "Bad parse") M.fromList . parseOnly (pRow `sepBy1'` endOfLine)
  where
    pRow = (,) <$> pName <*> (": " *> pExpr)
    pName = take 4
    pExpr = choice [Number <$> decimal, pOperation]
    pOperation = Operation <$> pName <*> (skipSpace *> pOp <* skipSpace) <*> pName
    pOp = choice [Plus <$ "+", Minus <$ "-", Mul <$ "*", Div <$ "/"]

part1 :: Map Text Expr -> Word64
part1 = calc "root"
  where
    calc m monkeys = case monkeys M.! m of
        (Number n) -> n
        (Operation x o y) -> apply o (calc x monkeys) (calc y monkeys)

part2 :: Map Text Expr -> Symbolic SBool
part2 monkeys = do
    let (left, right) = case monkeys M.! "root" of
            Number _ -> error "Not the correct form"
            (Operation x _ y) -> (x, y)
    humn <- sWord64 "humn"
    let follow m
            | m == "humn" = humn
            | otherwise = case monkeys M.! m of
                (Number n) -> fromIntegral n
                (Operation x o y) -> apply o (follow x) (follow y)
    pure $ follow left .== follow right

main :: IO ()
main = do
    input <- readMonkeys <$> readFile "input.txt"
    print $ part1 input
    allSat (part2 input) >>= print

{-
For part 2:

cabal run | rg 'humn = (\d+) :: Word64' -r '$1' | sort -n | head -n 1

-}
