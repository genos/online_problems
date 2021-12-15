{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Bool            (bool)
import           Data.Either          (fromRight)
import           Data.Foldable        (find, traverse_)
import           Data.Matrix          (Matrix)
import qualified Data.Matrix          as M
import           Data.Text            (Text)
import qualified Data.Text.IO         as T

data BingoCard = BC (Matrix Bool) (Matrix Int)
type Game = ([Int], [BingoCard])

len :: Int
len = 5

gameP :: Text -> Game
gameP = fromRight (error "Bad parse") . parseOnly ((,) <$> (nums <* "\n") <*> many1' card)
 where
  nums = decimal `sepBy1'` ","
  card = bc . M.fromLists <$> count len (count len (skipSpace *> decimal <* skipSpace))
  bc   = BC (M.matrix len len $ const False)

indices :: [Int]
indices = [1 .. len]

pairs :: [(Int, Int)]
pairs = [ (i, j) | i <- indices, j <- indices ]

won :: BingoCard -> Bool
won (BC marks _) = rows || cols || diags
 where
  rows  = or [ and (i `M.getRow` marks) | i <- indices ]
  cols  = or [ and (i `M.getCol` marks) | i <- indices ]
  diags = and (M.getDiag marks) || and (M.getDiag $ M.transpose marks)

mark :: Int -> BingoCard -> BingoCard
mark n (BC marks card) = BC m' card
 where
  m' = maybe marks (flip (M.setElem True) marks) $ find ((n ==) . (card M.!)) pairs

score :: Int -> BingoCard -> Int
score n (BC mask card) = n * sum unmarked
  where unmarked = zipWith (\m c -> bool c 0 m) (M.toList mask) (M.toList card)

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
part2 (n : ns, [bc]) | won bc'   = Just $ score n bc'
                     | otherwise = part2 (ns, [bc'])
  where bc' = mark n bc
part2 (n : ns, bcs) = part2 (ns, filter (not . won) $ mark n <$> bcs)

main :: IO ()
main = do
  input <- gameP <$> T.readFile "input.txt"
  traverse_ (print . ($ input)) [part1, part2]
