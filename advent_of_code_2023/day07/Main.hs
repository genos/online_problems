{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List (find, group, sort, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as T

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum)

type Hand = [Card] -- length is exactly 5

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord)

handType :: Hand -> HandType
handType h =
    let groups = group $ sort h
        n = length groups
        ns = sort $ fmap length groups
     in case n of
            1 -> FiveOfAKind
            2 -> case ns of
                [1, 4] -> FourOfAKind
                [2, 3] -> FullHouse
                _ -> error $ "Impossible lengths: " <> show ns
            3 -> case ns of
                [1, 1, 3] -> ThreeOfAKind
                [1, 2, 2] -> TwoPair
                _ -> error $ "Impossible lengths: " <> show ns
            4 -> OnePair
            5 -> HighCard
            _ -> error $ "Impossible length: " <> show n

readGame :: Text -> [(Hand, Word)]
readGame = either (error "Bad parse") id . parseOnly ((handAndBid `sepBy1'` "\n") <* endOfInput)
  where
    handAndBid = (,) <$> hand <*> (skipSpace *> decimal)
    hand = count 5 card
    card = choice $ zipWith ($>) (char <$> "23456789TJQKA") [Two .. Ace]

solve :: (Card -> Card -> Ordering) -> (Hand -> Hand) -> [(Hand, Word)] -> Word
solve cmp f =
    sum
        . zipWith (*) [1 ..]
        . fmap (\(_, _, b) -> b)
        . sortBy (\(t0, h0, _) (t1, h1, _) -> cmp' (t0, h0) (t1, h1))
        . fmap (\(h, b) -> (handType (f h), h, b))
  where
    cmp' (t0, h0) (t1, h1)
        | t0 > t1 = GT
        | t0 < t1 = LT
        | otherwise = fromMaybe EQ . find (/= EQ) $ zipWith cmp h0 h1

part1 :: [(Hand, Word)] -> Word
part1 = solve compare id

part2 :: [(Hand, Word)] -> Word
part2 = solve cmp f
  where
    f [a, b, c, d, e] =
        last $
            sortOn
                handType
                [[a', b', c', d', e'] | a' <- repJ a, b' <- repJ b, c' <- repJ c, d' <- repJ d, e' <- repJ e]
    f _ = error "Bad hand"
    repJ c = if c == Jack then [Two .. Ace] else [c]
    cmp Jack Jack = EQ
    cmp Jack _ = LT
    cmp _ Jack = GT
    cmp x y = compare x y

main :: IO ()
main = do
    input <- readGame <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
