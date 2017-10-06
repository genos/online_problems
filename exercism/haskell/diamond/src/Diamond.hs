module Diamond (diamond) where

diamond :: Char -> [String]
diamond c | 'A' <= c && c <= 'Z' = lines ++ (drop 1 . reverse $ lines)
          | otherwise            = error "Bad character; should be in [A, Z]"
 where
  lines :: [String]
  lines = fmap (mkLine n) . zip [0 ..] $ chars
  n :: Int
  n = length chars
  chars :: String
  chars = enumFromTo 'A' c
  mkLine :: Int -> (Int, Char) -> String
  mkLine n (0, c) = s ++ [c] ++ s where s = replicate (n - 1) ' '
  mkLine n (k, c) = o ++ [c] ++ i ++ [c] ++ o
   where
    o = replicate (n - k - 1) ' '
    i = replicate (2 * k - 1) ' '
