module Main where
import qualified Control.Foldl as L
import Data.List (permutations)
mean = (/) <$> L.Fold (+) 0 fromIntegral <*> L.genericLength
numComp [] = 0
numComp [x] = 0
numComp (x:y:zs) | x > y = 1
                 | otherwise = 1 + numComp (y : zs)
brute = L.fold mean . map numComp . permutations
main = print $ brute [1 .. 10]
