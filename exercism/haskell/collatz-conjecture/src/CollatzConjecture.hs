module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n | n < 1     = Nothing
          | otherwise = Just $ c n 0
 where
  c 1 l = l
  c k l = c (if even k then k `div` 2 else 3 * k + 1) (succ l)
