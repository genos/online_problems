{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified Data.Attoparsec.Text as P
import           Data.Bifunctor       (second)
import           Data.Char            (ord)
import           Data.Foldable        (foldl')
import           Data.List            (partition)
import qualified Data.Map             as M
import           Data.Map.Strict      (Map)
import           Data.Set             (Set)
import qualified Data.Set             as S
import qualified Data.Text.IO         as T

type Dependencies = Map Char (Set Char)

lineP :: P.Parser [(Char, Set Char)]
lineP = do
  a <- P.string "Step " *> P.letter
  _ <- P.string " must be finished before step "
  b <- P.letter <* P.string " can begin."
  pure [(a, S.empty), (b, S.singleton a)]

parseInput :: IO Dependencies
parseInput = do
  input <- T.readFile "input"
  case P.parseOnly (lineP `P.sepBy` P.char '\n') input of
    Left  l -> error l
    Right r -> pure . M.fromListWith S.union $ concat r

clear :: Dependencies -> Char -> Dependencies
clear deps v = M.map (S.delete v) $ M.delete v deps

nextReady :: Dependencies -> Char
nextReady = fst . M.findMin . M.filter S.null

part1 :: Dependencies -> String  -- lexicographic-topological sort
part1 = go ""
 where
  go str deps | M.null deps = reverse str
              | otherwise   = go (v : str) (clear deps v)
    where v = nextReady deps

part2 :: Dependencies -> Int
part2 deps = go deps (replicate 5 worker) 0
 where
  worker  = (' ', -1)
  go !ds !workers !time
    | M.null ds = time + maximum (fmap snd workers)
    | otherwise =
      let elapsed            = min0 $ filter (> 0) (fmap snd workers)
          workers'           = fmap (second $ subtract elapsed) workers
          (done, working)    = partition ((<= 0) . snd) workers'
          cleared            = foldl' clear ds $ fmap fst done
          (workers'', deps') = assign (working, cleared) (length done)
      in  go deps' workers'' (time + elapsed)
   where
    min0 ts = if null ts then 0 else minimum ts
    assign (w, d) n
      | n <= 0                     = (w, d)
      | M.null $ M.filter S.null d = assign (worker : w, d) n'
      | otherwise                  = assign ((v, c) : w, M.delete v d) n'
      where v = nextReady d
            c = ord v - 4
            n' = n - 1

main :: IO ()
main = do
  deps <- parseInput
  putStrLn $ part1 deps
  print $ part2 deps
