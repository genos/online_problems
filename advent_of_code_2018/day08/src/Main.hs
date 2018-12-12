module Main where

import           Control.Lens         ((^?), ix)
import qualified Data.Attoparsec.Text as P
import           Data.Maybe           (mapMaybe)
import qualified Data.Text.IO         as T

part1 :: P.Parser Int
part1 = do
  numKids <- P.decimal <* P.skipSpace
  numMeta <- P.decimal <* P.skipSpace
  kids    <- sum <$> P.count numKids (part1 <* P.skipSpace)
  meta    <- sum <$> P.count numMeta (P.decimal <* P.skipSpace)
  pure $! kids + meta

part2 :: P.Parser Int
part2 = do
  numKids <- P.decimal <* P.skipSpace
  numMeta <- P.decimal <* P.skipSpace
  kids    <- P.count numKids (part2 <* P.skipSpace)
  meta    <- P.count numMeta (P.decimal <* P.skipSpace)
  pure $! if null kids
          then sum meta
          else sum . mapMaybe (\i -> kids ^? ix (i - 1)) $! meta

main :: IO ()
main = do
  input <- T.readFile "input"
  print $ P.parseOnly part1 input
  print $ P.parseOnly part2 input
