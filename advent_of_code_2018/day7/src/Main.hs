{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow        ((&&&))
import qualified Data.Attoparsec.Text as P
import           Data.Char            (ord)
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

part1 :: Dependencies -> String  -- lexicographic-topological sort
part1 = go ""
 where
  go str deps | M.null deps  = reverse str
              | otherwise    = go (v : str) deps'
   where
    v     = fst . M.findMin . M.filter S.null $ deps
    deps' = M.map (S.delete v) $ M.delete v deps

part2 :: Dependencies -> Int
part2 = go 0 . (assign 0)
 where
  assign :: Int -> Dependencies -> (Map Char Int, Dependencies)
  assign n ds = (working, rest)
    where
      working = M.mapWithKey cost . M.take (5 - n) . M.filter S.null $ ds
      rest = _todo
  cost :: Char -> Set Char -> Int
  cost key _ = ord key - 4
  go :: Int -> (Map Char Int, Dependencies) -> Int
  go time (workers, deps) | M.null deps = time + (maximum workers)
                          | otherwise   = go time' (workers', deps')
   where
     elapsed           = if null workers then 0 else minimum workers
     time'             = time + elapsed
     (done, working)   = M.partition (<= 0) $ M.map (subtract elapsed) workers
     clear             = M.keysSet done
     (starting, deps') = assign (length working) deps
     workers'          = M.union working starting

main :: IO ()
main = do
  deps <- parseInput
  putStrLn . part1 $ deps
  print . part2 $ deps
