{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Linear.V2 (V2 (..))

data Square = Bug | Empty deriving (Eq, Show)

type Grid = Map (V2 Word) Square

toC :: Square -> Char
toC = \case Bug -> '#'; Empty -> '.'

fromC :: Char -> Maybe Square
fromC = \case '#' -> Just Bug; '.' -> Just Empty; _ -> Nothing

parse :: String -> Grid
parse s = M.fromList $ zip coords squares
  where
    ls = lines s
    n = fromIntegral $ length ls - 1
    coords = [V2 x y | x <- [0 .. n], y <- [0 .. n]]
    squares = concatMap (mapMaybe fromC) ls

readInput :: FilePath -> IO Grid
readInput = (parse <$>) . readFile

main :: IO ()
main = putStrLn "TODO"
