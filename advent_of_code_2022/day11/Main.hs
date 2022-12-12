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
import Data.Word (Word64)
import Deque.Strict (Deque)
import qualified Deque.Strict as D

data Monkey = M
    { _number :: {-# UNPACK #-} !Int
    , _worries :: Deque Word64
    , _operation :: Word64 -> Word64
    , _divBy :: !Word64
    , _true :: {-# UNPACK #-} !Int
    , _false :: {-# UNPACK #-} !Int
    , _inspected :: !Word64
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

catch :: Word64 -> Monkey -> Monkey
catch w (M n ws o d t f i) = M n (D.snoc w ws) o d t f i

turn :: (Word64 -> Word64) -> Vector Monkey -> Int -> Vector Monkey
turn g ms n = ms'
  where
    M k ws op d t f i = ms V.! n
    m0 = M k mempty op d t f (i + fromIntegral (length ws))
    ms' = V.modify (\v -> M.write v n m0) $ foldl' h ms ws
    h mz w =
        let w' = g $ op w
            n' = bool f t $ 0 == (w' `mod` d)
            m1 = catch w' $ mz V.! n'
         in V.modify (\v -> M.write v n' m1) mz

gameRound :: (Word64 -> Word64) -> Vector Monkey -> Vector Monkey
gameRound g ms = V.foldl' (turn g) ms . V.enumFromTo 0 . pred $ V.length ms

solve :: Word64 -> (Word64 -> Word64) -> Vector Monkey -> Word64
solve n g = product . take 2 . reverse . sort . V.toList . fmap _inspected . go 0
  where
    go k ms
        | k == n = ms
        | otherwise = go (succ k) (gameRound g ms)

main :: IO ()
main = do
    monkeys <- readMonkeys <$> T.readFile "input.txt"
    let modulus = product $ _divBy <$> monkeys
    traverse_ (print . ($ monkeys)) [solve 20 (`div` 3), solve 10000 (`mod` modulus)]
