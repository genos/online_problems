module Main
  ( main
  ) where
import Data.Attoparsec.Text hiding (count)
import Data.Char            (isAlpha)
import Data.Text            (count, index)

data Rule = Rule
  { _lower    :: {-# UNPACK #-}!Int
  , _upper    :: {-# UNPACK #-}!Int
  , _char     :: {-# UNPACK #-}!Char
  , _password :: !Text
  }

rule :: Parser Rule
rule =
  Rule
    <$> decimal
    <*> (char '-' *> decimal)
    <*> (skipSpace *> letter)
    <*> (char ':' *> skipSpace *> takeWhile1 isAlpha)

input :: IO [Rule]
input =
  fromRight []
    .   parseOnly ((rule `sepBy1'` char '\n') <* skipSpace <* endOfInput)
    <$> readFileText "input.txt"

part1 :: [Rule] -> Int
part1 = length . filter policy
  where policy (Rule l u c p) = let n = count (one c) p in l <= n && n <= u

part2 :: [Rule] -> Int
part2 = length . filter policy where
  policy (Rule l u c p) = (x == c && y /= c) || (x /= c && y == c)
   where
    x = p `index` (l - 1)
    y = p `index` (u - 1)

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
