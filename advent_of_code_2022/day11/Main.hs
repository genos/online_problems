{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.Bool (bool)
import Data.Foldable (foldl', traverse_)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Deque.Strict (Deque)
import qualified Deque.Strict as D

data Monkey = M
    { _number :: !Integer
    , _worries :: Deque Integer
    , _operation :: Integer -> Integer
    , _divBy :: {-# UNPACK #-} !Integer
    , _true :: {-# UNPACK #-} !Int
    , _false :: {-# UNPACK #-} !Int
    , _inspected :: {-# UNPACK #-} !Integer
    }

instance Show Monkey where
    show (M n ws _o d t f i) = "Monkey<" <> show n <> ", " <> show ws <> ", " <> show d <> ", " <> show t <> ", " <> show f <> ", " <> show i <> ">"

readMonkeys :: Text -> Vector Monkey
readMonkeys = either (error "Bad Parse") V.fromList . parseOnly (m `sepBy1'` endOfLine)
  where
    m = M <$> header <*> items <*> operation <*> divBy <*> true <*> false <*> pure 0
    header = string "Monkey " *> decimal <* char ':' <* endOfLine
    items = (`D.fromConsAndSnocLists` []) <$> (skipSpace *> string "Starting items: " *> (decimal `sepBy'` string ", ") <* endOfLine)
    operation = skipSpace *> op <* endOfLine
    divBy = skipSpace *> string "Test: divisible by " *> decimal <* endOfLine
    true = skipSpace *> string "If true: throw to monkey " *> decimal <* endOfLine
    false = skipSpace *> string "If false: throw to monkey " *> decimal <* endOfLine
    op = do
        _ <- string "Operation: new = old "
        f <- choice [(+) <$ string "+ ", (*) <$ string "* "]
        choice [self f, other f]
    self f = (\n -> f n n) <$ string "old"
    other f = f <$> decimal

catch :: Integer -> Monkey -> Monkey
catch w (M n is o d t f i) = M n (D.snoc w is) o d t f i

turn :: Vector Monkey -> Int -> Vector Monkey
turn ms n = if null (_worries $ ms V.! n) then ms else ms'
  where
    M k ws op d t f i = ms V.! n
    m0 = M k mempty op d t f (i + fromIntegral (length ws))
    ms' = V.modify (\v -> M.write v n m0) $ foldl' g ms ws
    g mz w =
        let w' = op w `div` 3
            n' = bool f t $ 0 == (w' `mod` d)
            m1 = catch w' $ mz V.! n'
         in V.modify (\v -> M.write v n' m1) mz

gameRound :: Vector Monkey -> Vector Monkey
gameRound ms = foldl' turn ms [0 .. V.length ms - 1]

part1 :: Vector Monkey -> Integer
part1 ms = product . take 2 . reverse . sort . V.toList . fmap _inspected $ foldl' (\mz _ -> gameRound mz) ms [0 .. 19 :: Int]

main :: IO ()
main = do
    monkeys <- readMonkeys <$> T.readFile "input.txt"
    traverse_ (print . ($ monkeys)) [part1]
