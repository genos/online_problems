{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.List (intersect)
import Data.Text (Text)
import Data.Text.IO qualified as T

test :: Text
test =
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
    \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
    \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
    \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
    \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
    \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

data Card = C {_num :: Int, _winning :: [Int], _have :: [Int]}

readCards :: Text -> [Card]
readCards = either (error "Bad parse") id . parseOnly ((card `sepBy1'` "\n") <* endOfInput)
  where
    card = C <$> ("Card" *> skipSpace *> decimal <* ": ") <*> nums <*> (" | " *> nums)
    nums = (skipSpace *> decimal) `sepBy1'` " "

part1 :: [Card] -> Int
part1 = sum . fmap score
  where
    score (C _ w h) = let i = intersect w h in if null i then 0 else (2 ^) . pred $ length i

main :: IO ()
main = do
    print . part1 $ readCards test
    input <- readCards <$> T.readFile "input.txt"
    print $ part1 input
