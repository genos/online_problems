module Main where

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.List (findIndices, sort)
import Data.Text (Text)
import qualified Data.Text.IO as T

data Packet = One {-# UNPACK #-} !Int | List [Packet] deriving (Eq)

readPairs :: Text -> [(Packet, Packet)]
readPairs = either (error "Bad parse") id . parseOnly (pair `sepBy1'` (endOfLine *> endOfLine))
  where
    pair = (,) <$> packet <*> (endOfLine *> packet)
    packet = choice [One <$> decimal, List <$> (char '[' *> packet `sepBy'` char ',' <* char ']')]

instance Ord Packet where
    compare (One x) (One y) = compare x y
    compare (List xs) (List ys) = compare xs ys
    compare (One x) ys@(List _) = compare (List [One x]) ys
    compare xs@(List _) (One y) = compare xs (List [One y])

part1 :: [(Packet, Packet)] -> Int
part1 = sum . fmap succ . findIndices (uncurry (<))

part2 :: [(Packet, Packet)] -> Int
part2 = product . fmap succ . findIndices (`elem` twosix) . sort . (twosix ++) . concatMap (\(x, y) -> [x, y])
  where
    twosix = fmap (\x -> List [List [One x]]) [2, 6]

main :: IO ()
main = do
    input <- readPairs <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
