{-
Here's a Haskell version that gives two implementations: a linear update a la
the usual iterative Fibonacci procedure, and a (mutually) recursive one like
the last solution.
[sourcecode lang="css"]
-}
import Control.Arrow ((&&&))

lucas :: Integer -> Integer -> Integer -> Integer -> [Integer]
lucas x0 x1 p q = x0 : x1 : zipWith (\x y-> p * x - q * y) as bs
    where
        as = lucas x0 x1 p q
        bs = tail as

u1 :: Integer -> Integer -> [Integer]
u1 = lucas 0 1

v1 :: Integer -> Integer -> [Integer]
v1 p = lucas 2 p p

u2 :: Integer -> Integer -> Integer -> Integer
u2 p q n | n <= 0    = 0
         | n == 1    = 1
         | odd n     = u2 p q (k + 1) * v2 p q k - q^k
         | otherwise = u2 p q k * v2 p q k
         where
             k = n `div` 2

v2 :: Integer -> Integer -> Integer -> Integer
v2 p q n | n <= 0    = 2
         | n == 1    = p
         | odd n     = v2 p q (k + 1) * v2 p q k - p * q^k
         | otherwise = v2 p q k ^2  - 2 * q^k
         where
             k = n `div` 2

main :: IO ()
main = do
    let (p,q) = (1,-1)
    mapM_ (print . take 17) [u1 p q, v1 p q]
    print $ map (u2 p q &&& v2 p q) [0..16]
{-
[/sourcecode]
-}
