{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile, take)

import Control.Monad ((<=<))
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.SBV
import Data.SBV.Internals (CVal (..), cvVal, modelAssocs)
import Data.Text (Text)
import Data.Text.IO (readFile)

data Op = Plus | Minus | Mul | Div

apply :: (Num a, Fractional a) => Op -> a -> a -> a
apply = \case
    Plus -> (+)
    Minus -> (-)
    Mul -> (*)
    Div -> (\x y -> x * recip y) -- workaround a bug in SMTLib2 interface

data Expr = Number Rational | Operation Text Op Text

readMonkeys :: Text -> Map Text Expr
readMonkeys = either (error "Bad parse") M.fromList . parseOnly (pRow `sepBy1'` endOfLine)
  where
    pRow = (,) <$> pName <*> (": " *> pExpr)
    pName = take 4
    pExpr = choice [Number . (% 1) <$> decimal, pOperation]
    pOperation = Operation <$> pName <*> (skipSpace *> pOp <* skipSpace) <*> pName
    pOp = choice [Plus <$ "+", Minus <$ "-", Mul <$ "*", Div <$ "/"]

part1 :: Map Text Expr -> IO Integer
part1 = pure . numerator . calc "root"
  where
    calc m monkeys = case monkeys M.! m of
        (Number n) -> n
        (Operation x o y) -> apply o (calc x monkeys) (calc y monkeys)

part2 :: Map Text Expr -> IO Integer
part2 monkeys = answer
  where
    answer = do
        (SatResult (Satisfiable _ smtResult)) <- sat go
        pure . numerator $ case cvVal . snd . head $ modelAssocs smtResult of
            (CRational r) -> r
            _ -> error "No rational value found."
    go = do
        let (left, right) = case monkeys M.! "root" of
                Number _ -> error "Not the correct form"
                (Operation x _ y) -> (x, y)
        humn <- sRational "humn"
        let follow m
                | m == "humn" = humn
                | otherwise = case monkeys M.! m of
                    (Number n) -> fromRational n
                    (Operation x o y) -> apply o (follow x) (follow y)
        pure (follow left .== follow right)

main :: IO ()
main = do
    input <- readMonkeys <$> readFile "input.txt"
    traverse_ (print <=< ($ input)) [part1, part2]
