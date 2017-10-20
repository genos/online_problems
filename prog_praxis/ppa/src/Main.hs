module Main where

ppa :: Num a => [a] -> [a]
ppa = zipWith (*) <$> init . scanl (*) 1 <*> tail . scanr (*) 1

main :: IO ()
main = print . ppa $ [1 .. 5 :: Int]
