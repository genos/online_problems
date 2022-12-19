-- with help from https://github.com/juanplopes/advent-of-code-2022/blob/main/day16.py
-- and https://topaz.github.io/paste/#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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.State.Strict
import Data.Attoparsec.Text as A
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T

type Name = Text
data Valve = Valve {_flow :: {-# UNPACK #-} !Int, _tunnels :: Set Name}
makeLenses ''Valve

readValves :: Text -> Map Name Valve
readValves = either (error "Bad Parse") M.fromList . parseOnly (pValve `sepBy1'` endOfLine)
  where
    pValve = pair <$> ("Valve " *> pName) <*> pFlow <*> (S.fromList <$> pTunnels)
    pName = A.take 2
    pFlow = " has flow rate=" *> decimal
    pTunnels = "; " *> choice ["tunnel leads to valve ", "tunnels lead to valves "] *> (pName `sepBy'` ", ")
    pair n i ns = (n, Valve i ns)

adjacency :: Map Name Valve -> Map (Name, Name) Int
adjacency m = M.map fromJust $ go (M.fromList [((x, y), edge x y) | x <- keys, y <- keys]) keys
  where
    min' (Just x) (Just y) = Just $ min x y
    min' (Just x) Nothing = Just x
    min' Nothing (Just y) = Just y
    min' Nothing Nothing = Nothing
    (.+.) :: Num a => Maybe a -> Maybe a -> Maybe a
    (Just x) .+. (Just y) = Just $ x + y
    _ .+. _ = Nothing
    edge x y = if y `S.member` (m ^?! ix x . tunnels) then Just 1 else Nothing
    keys = M.keys m
    keyPairs = let ks = M.keysSet m in S.cartesianProduct ks ks
    go = foldl' (\adj k -> foldl' (f k) adj keyPairs)
      where
        f k a (i, j) =
            let aij = a M.! (i, j)
                aik = a M.! (i, k)
                akj = a M.! (k, j)
             in M.insert (i, j) (min' aij (aik .+. akj)) a

search :: Int -> Map Name Valve -> Map (Set Name) Int
search time valves = execState (go time S.empty 0 "AA") M.empty
  where
    adj = adjacency valves
    nonZero = M.filter ((0 <) . _flow) valves
    go t open f v
        | t <= 0 = pure ()
        | otherwise = do
            let cache = \case Nothing -> Just f; (Just old) -> Just (max f old)
            modify' $ M.alter cache open
            let unvisited = M.withoutKeys nonZero open
                visit u =
                    let t' = t - (adj M.! (v, u)) - 1
                        f' = f + t' * (valves ^?! ix u . flow)
                     in go t' (S.insert u open) f' u
            mapM_ visit (M.keys unvisited)

part1 :: Map Name Valve -> Int
part1 = maximum . M.elems . search 30

main :: IO ()
main = do
    input <- readValves <$> T.readFile "input.txt"
    print $ part1 input
