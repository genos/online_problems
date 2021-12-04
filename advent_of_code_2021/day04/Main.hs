{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (find)
import           Data.Matrix          (Matrix)
import qualified Data.Matrix          as M
import           Data.Text            (Text)
import qualified Data.Text.IO         as T

data BingoCard = BC (Matrix Bool) (Matrix Int) deriving Show
type Game = ([Int], [BingoCard])

gameP :: Text -> Game
gameP = fromRight (error "PARSE") . parseOnly ((,) <$> (nums <* "\n") <*> many1' card)
 where
  nums = decimal `sepBy1'` ","
  card = toBC . M.fromLists <$> count 5 (count 5 (skipSpace *> decimal <* skipSpace))
  toBC = BC (M.matrix 5 5 $ const False)

won :: BingoCard -> Bool
won (BC marks _) = rows || cols || diags
 where
  rows  = or [ and (M.getRow i marks) | i <- [1 .. 5] ]
  cols  = or [ and (M.getCol i marks) | i <- [1 .. 5] ]
  diags = and (M.getDiag marks) || and (M.getDiag $ M.transpose marks)

mark :: Int -> BingoCard -> BingoCard
mark n (BC marks card) = BC marks' card
 where
  marks' =
    case find (\ij -> (n ==) $ card M.! ij) [ (i, j) | i <- [1 .. 5], j <- [1 .. 5] ] of
      Nothing     -> marks
      Just (i, j) -> M.setElem True (i, j) marks

score :: Int -> BingoCard -> Int
score n (BC mask card) = n * sum unmarked
  where unmarked = zipWith (\m c -> if m then 0 else c) (M.toList mask) (M.toList card)

part1 :: Game -> Maybe Int
part1 (_ , []) = Nothing
part1 ([], _ ) = Nothing
part1 (n : ns, bcs) =
  let bcs' = fmap (mark n) bcs
  in  case find won bcs' of
        Nothing -> part1 (ns, bcs')
        Just bc -> Just $ score n bc

part2 :: Game -> Maybe Int
part2 (_ , []) = Nothing
part2 ([], _ ) = Nothing
part2 (n : ns, bcs) = _todo

main :: IO ()
main = do
  test <- gameP <$> T.readFile "input.txt"
  print $ part1 test
