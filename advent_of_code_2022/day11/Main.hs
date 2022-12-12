{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Attoparsec.Text hiding (take)
import Data.Bool (bool)
import Data.Foldable (foldl', traverse_)
import Data.List (sort)
import Data.Sequence (Seq, fromList)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V

data Monkey = Monkey
    { _worries :: Seq Int
    , _operation :: Int -> Int
    , _divBy :: {-# UNPACK #-}!Int
    , _true :: {-# UNPACK #-} !Int
    , _false :: {-# UNPACK #-} !Int
    , _inspected :: {-# UNPACK #-}!Int
    }

$(makeLenses ''Monkey)

readMonkeys :: Text -> Vector Monkey
readMonkeys = either (error "Bad Parse") V.fromList . parseOnly (m `sepBy1'` endOfLine)
  where
    m = Monkey <$> (h *> w) <*> o <*> d <*> t <*> f <*> pure 0
    h = string "Monkey " *> skipWhile (/= '\n') *> endOfLine
    w = fromList <$> (skipSpace *> string "Starting items: " *> (decimal `sepBy'` string ", ") <* endOfLine)
    o = skipSpace *> parseOp <* endOfLine
    d = skipSpace *> string "Test: divisible by " *> decimal <* endOfLine
    t = skipSpace *> string "If true: throw to monkey " *> decimal <* endOfLine
    f = skipSpace *> string "If false: throw to monkey " *> decimal <* endOfLine
    parseOp = do
        _ <- string "Operation: new = old "
        b <- choice [(+) <$ string "+ ", (*) <$ string "* "]
        choice [self b, other b]
    self b = (\x -> b x x) <$ string "old"
    other = (<$> decimal)

turn :: (Int -> Int) -> Vector Monkey -> Int -> Vector Monkey
turn g ms n = ms'
  where
    m = ms `V.unsafeIndex` n
    m' = m & worries .~ mempty & inspected +~ (m ^. worries & length)
    ms' = foldl' h ms (_worries m) & ix n .~ m'
    h mz w =
        let w' = g $ _operation m w
            n' = bool (_false m) (_true m) $ 0 == (w' `mod` _divBy m)
        in mz & (ix n' . worries) %~ (|> w')

solve :: Int -> (Int -> Int) -> Vector Monkey -> Int
solve n g = product . take 2 . reverse . sort . (^.. traverse . inspected) . outer 0
  where
    outer k ms
        | k == n = ms
        | otherwise = outer (succ k) (inner ms)
    inner ms = V.foldl' (turn g) ms . V.enumFromN 0 $ V.length ms

main :: IO ()
main = do
    monkeys <- readMonkeys <$> T.readFile "input.txt"
    let modulus = product $ _divBy <$> monkeys
    traverse_ (print . ($ monkeys)) [solve 20 (`div` 3), solve 10000 (`mod` modulus)]
