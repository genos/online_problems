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
import Data.Word (Word64)

data Monkey = M
    { _number :: {-# UNPACK #-} !Int
    , _worries :: Seq Word64
    , _operation :: Word64 -> Word64
    , _divBy :: {-# UNPACK #-}!Word64
    , _true :: {-# UNPACK #-} !Int
    , _false :: {-# UNPACK #-} !Int
    , _inspected :: {-# UNPACK #-}!Int
    }

$(makeLenses ''Monkey)

readMonkeys :: Text -> Vector Monkey
readMonkeys = either (error "Bad Parse") V.fromList . parseOnly (m `sepBy1'` endOfLine)
  where
    m = M <$> n <*> w <*> o <*> d <*> t <*> f <*> pure 0
    n = string "Monkey " *> decimal <* char ':' <* endOfLine
    w = fromList <$> (skipSpace *> string "Starting items: " *> (decimal `sepBy'` string ", ") <* endOfLine)
    o = skipSpace *> parseOp <* endOfLine
    d = skipSpace *> string "Test: divisible by " *> decimal <* endOfLine
    t = skipSpace *> string "If true: throw to monkey " *> decimal <* endOfLine
    f = skipSpace *> string "If false: throw to monkey " *> decimal <* endOfLine
    parseOp = do
        _ <- string "Operation: new = old "
        g <- choice [(+) <$ string "+ ", (*) <$ string "* "]
        choice [self g, other g]
    self g = (\x -> g x x) <$ string "old"
    other g = g <$> decimal

turn :: (Word64 -> Word64) -> Vector Monkey -> Int -> Vector Monkey
turn g ms n = ms'
  where
    m = ms ^?! ix n
    m' = m & worries .~ mempty & inspected +~ (m ^. worries & length)
    ms' = foldl' h ms (_worries m) & ix n .~ m'
    h mz w =
        let w' = g $ _operation m w
            n1 = bool (_false m) (_true m) $ 0 == (w' `mod` _divBy m)
            m1 = (mz V.! n1) & worries %~ (|> w')
         in mz & ix n1 .~ m1

gameRound :: (Word64 -> Word64) -> Vector Monkey -> Vector Monkey
gameRound g ms = V.foldl' (turn g) ms . V.enumFromTo 0 . pred $ V.length ms

solve :: Word64 -> (Word64 -> Word64) -> Vector Monkey -> Int
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
