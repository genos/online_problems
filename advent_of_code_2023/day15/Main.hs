{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.ST (runST)
import Data.Attoparsec.Text
import Data.Char (ord)
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as M

test :: Text
test = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

hash :: Text -> Int
hash = T.foldl' (\h c -> (17 * (h + ord c)) `rem` 256) 0

part1 :: Text -> Int
part1 = sum . fmap hash . T.split (== ',')

data Op = Dash | Equal Int deriving (Show)
type Step = (Text, Op)

readSteps :: Text -> [Step]
readSteps = either (error "Bad parse") id . parseOnly ((step `sepBy1'` ",") <* endOfInput)
  where
    step = (,) <$> label <*> op
    label = T.pack <$> many1' letter
    op = (Dash <$ "-") <|> (Equal <$> ("=" *> decimal))

set :: (Text, Int) -> [(Text, Int)] -> [(Text, Int)]
set (a, m) [] = [(a, m)]
set (a, m) ((b, n) : bs)
    | a == b = (a, m) : bs
    | otherwise = (b, n) : set (a, m) bs

perform :: Vector [(Text, Int)] -> [Step] -> Vector [(Text, Int)]
perform b ss = runST $ do
    b' <- V.unsafeThaw b
    for_ ss $ \(label, op) ->
        let f = case op of
                Dash -> filter ((/= label) . fst)
                Equal i -> set (label, i)
         in M.unsafeModify b' f (hash label)
    V.unsafeFreeze b'

part2 :: Text -> Int
part2 =
    V.sum
        . V.imap (\i b -> sum $ zipWith (\j (_l, k) -> (1 + i) * j * k) [1 ..] b)
        . perform (V.replicate 256 [])
        . readSteps

main :: IO ()
main = do
    traverse_ (print . ($ test)) [part1, part2]
    input <- T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
