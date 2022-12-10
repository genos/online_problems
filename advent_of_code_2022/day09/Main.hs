module Main where

import Data.Attoparsec.Text
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.List (nub, scanl', sort)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Linear.V2

readCoords :: Text -> [V2 Int]
readCoords = either (error "Bad parse") (scanl' (+) 0 . concat) . parseOnly (vs `sepBy1'` endOfLine)
  where
    vs = flip replicate <$> v <*> (skipSpace *> decimal)
    v = choice [V2 0 1 <$ char 'U', V2 0 (-1) <$ char 'D', V2 (-1) 0 <$ char 'L', V2 1 0 <$ char 'R']

-- some inspiration from https://blog.jle.im/
setup :: [V2 Int] -> [[V2 Int]]
setup = iterate (scanl' follow 0)
  where
    follow t h = let d = (h - t) in t + bool 1 0 (maximum (abs d) < 2) * signum d

solve :: Int -> [[V2 Int]] -> Int
solve n = length . nub . sort . (!! n)

main :: IO ()
main = do
    paths <- setup . readCoords <$> T.readFile "input.txt"
    traverse_ (print . ($ paths)) [solve 1, solve 9]
