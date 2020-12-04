module Main
  ( main
  ) where

import           Data.Attoparsec.Text
import           Data.Char            (isHexDigit)
import           Data.HashSet         (isSubsetOf, member)
import qualified Data.Text            as T
import           Relude.Extra         ((!?))

input :: IO [[Text]]
input = fmap words . T.splitOn "\n\n" <$> readFileText "input.txt"

allRequired :: [Text] -> Bool
allRequired =
  isSubsetOf (fromList $ words "byr iyr eyr hgt hcl ecl pid") . fromList . fmap
    (T.takeWhile (/= ':'))

part1 :: [[Text]] -> Int
part1 = length . filter allRequired

between :: Int -> Int -> (Int -> Bool)
between lo hi = (&&) <$> (>= lo) <*> (<= hi)

rules :: HashMap Text (Text -> Bool)
rules = fromList
  [ ("byr", maybe False (between 1920 2002) . readMaybe . toString)
  , ("iyr", maybe False (between 2010 2020) . readMaybe . toString)
  , ("eyr", maybe False (between 2020 2030) . readMaybe . toString)
  , ("hgt", parses (cm <|> inch))
  , ("hcl", parses (char '#' *> count 6 (satisfy isHexDigit)))
  , ("ecl", (`member` (fromList $ words "amb blu brn gry grn hzl oth")))
  , ("pid", parses (count 9 digit))
  , ("cid", const True)
  ]
 where
  parses p = isRight . parseOnly (p <* endOfInput)
  cm   = (guard . between 150 193) =<< decimal <* string "cm"
  inch = (guard . between 59 76) =<< decimal <* string "in"


part2 :: [[Text]] -> Int
part2 = length . filter ((&&) <$> allRequired <*> all clears)
 where
  clears =
    uncurry ($)
      . bimap (fromMaybe (const False) . (rules !?)) T.tail
      . T.breakOn ":"

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
