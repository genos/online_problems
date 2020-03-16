{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Linear.V2 (V2 (..))

data Square = Bug | Empty deriving (Eq, Show)

type Grid = Map (V2 Int) Square

toC :: Square -> Char
toC = \case Bug -> '#'; Empty -> '.'

fromC :: Char -> Maybe Square
fromC = \case '#' -> Just Bug; '.' -> Just Empty; _ -> Nothing

parse :: String -> Grid
parse s = M.fromList $ zip coords squares
  where
    ls = lines s
    n = fromIntegral $ length ls - 1 -- assumes square grid…
    coords = [V2 x y | x <- [0 .. n], y <- [0 .. n]]
    squares = concatMap (mapMaybe fromC) ls

readInput :: FilePath -> IO Grid
readInput = (parse <$>) . readFile

pretty :: Grid -> String
pretty g = unlines ss
  where
    V2 x y = fst $ M.findMax g
    coords = [[V2 x' y' | y' <- [0 .. y]] | x' <- [0 .. x]]
    ss = fmap (fmap (toC . (g M.!))) coords

next :: Grid -> Grid
next g = M.mapWithKey update g
  where
    update v s
      | s == Bug && n == 1 = Bug
      | s == Empty && (n == 1 || n == 2) = Bug
      | otherwise = Empty
      where
        adj = [v + w | w <- [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]]
        n = length . filter (== Bug) $ mapMaybe (g M.!?) adj

main :: IO ()
main = putStrLn . pretty =<< readInput "input_1"
