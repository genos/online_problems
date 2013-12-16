facMod :: Integer -> Integer -> Integer
facMod n m = loop n 1
  where loop 0 acc = acc
        loop k f | k >= m     = 0
                 | f == 0     = 0
                 | otherwise  = loop (k - 1) ((f * k) `mod` m)

main :: IO ()
main = print $ 1000000 `facMod` 1001001779
